{-# LANGUAGE DataKinds #-}
module VerificationBench where

import           Universum

import           Control.DeepSeq (force)
import           Control.Monad.Random.Strict (evalRandT)
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import           Data.Time.Units (Microsecond, convertUnit)
import           Formatting (int, sformat, shown, (%))
import qualified Options.Applicative as Opts
import           System.Directory (doesFileExist)
import           System.Random (newStdGen)
import           System.Wlog (LoggerConfig, LoggerName (..), consoleActionB,
                     debugPlus, defaultHandleAction, logError, logInfo,
                     setupLogging, termSeveritiesOutB)

import           Mockable.CurrentTime (realTime)

import           Pos.AllSecrets (mkAllSecretsSimple)
import           Pos.Binary.Class (BiExtRep (..), DecoderAttrKind (..),
                     EitherExtRep (..), NonEmptyExtRep (..), decodeFull',
                     fillExtRep, serialize')
import           Pos.Block.Error (ApplyBlocksException, VerifyBlocksException)
import           Pos.Block.Logic.VAR (getVerifyBlocksContext',
                     verifyAndApplyBlocks, verifyBlocksPrefix, rollbackBlocks)
import           Pos.Core (Block, GenericBlock, GenesisBlockchain,
                     MainBlockchain)
import           Pos.Core.Chrono (NE, OldestFirst (..), nonEmptyNewestFirst)
import           Pos.Core.Common (BlockCount (..), unsafeCoinPortionFromDouble)
import           Pos.Core.Configuration (genesisBlockVersionData, genesisData,
                     genesisSecretKeys, slotSecurityParam)
import           Pos.Core.Genesis (FakeAvvmOptions (..), GenesisData (..),
                     GenesisInitializer (..), TestnetBalanceOptions (..))
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Crypto.Configuration (ProtocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Generator.Block (BlockGenParams (..), TxGenParams (..),
                     genBlocks)
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     HasConfigurations, defaultConfigurationOptions,
                     withConfigurationsM)
import           Pos.Txp.Logic.Global (txpGlobalSettings)
import           Pos.Util.CompileInfo (withCompileInfo)

import           Test.Pos.Block.Logic.Mode (BlockTestMode, TestParams (..),
                     runBlockTestMode)
import           Test.Pos.Block.Logic.Util (satisfySlotCheck)

genesisInitializer :: GenesisInitializer
genesisInitializer = GenesisInitializer
    { giTestBalance = balance
    , giFakeAvvmBalance = FakeAvvmOptions
          { faoCount = 1
          , faoOneBalance = maxBound
          }
    , giAvvmBalanceFactor = unsafeCoinPortionFromDouble 0
    , giUseHeavyDlg = False
    , giSeed = 0
    }

balance :: TestnetBalanceOptions
balance = TestnetBalanceOptions
    { tboPoors = 1
    , tboRichmen = 1
    , tboTotalBalance = maxBound
    , tboRichmenShare = 1
    , tboUseHDAddresses = False
    }

generateBlocks :: HasConfigurations => ProtocolMagic -> BlockCount -> BlockTestMode (OldestFirst NE (Block 'AttrExtRep))
generateBlocks pm bCount = do
    g <- liftIO $ newStdGen
    let secretKeys =
            case genesisSecretKeys of
                Nothing ->
                    error "generateBlocks: no genesisSecretKeys"
                Just ks -> ks
    bs <- flip evalRandT g $ genBlocks pm
            (BlockGenParams
                { _bgpSecrets = mkAllSecretsSimple secretKeys
                , _bgpBlockCount = bCount
                , _bgpTxGenParams = TxGenParams
                    { _tgpTxCountRange = (0, 2)
                    , _tgpMaxOutputs = 2
                    }
                , _bgpInplaceDB = False
                , _bgpSkipNoKey = True
                , _bgpGenStakeholders = gdBootStakeholders genesisData
                , _bgpTxpGlobalSettings = txpGlobalSettings pm
                })
            (maybeToList . fmap fst)
    return $ OldestFirst $ NE.fromList $ map blockFillExtRep bs
    where
        blockFillExtRep :: Block 'AttrNone -> Block 'AttrExtRep
        blockFillExtRep =  runEitherExtRep . fromRight (error "fillExtRep' failed") . fillExtRep . EitherExtRep


data BenchArgs = BenchArgs
    { baConfigPath :: FilePath
    , baConfigKey  :: Text
    , baBlockCount :: BlockCount
    , baRuns       :: Int
    , baApply      :: Bool
    , baBlockCache :: Maybe FilePath
    }

configPathP :: Opts.Parser FilePath
configPathP = Opts.strOption $
       Opts.long "config"
    <> Opts.value "lib/configuration.yaml"
    <> Opts.showDefault
    <> Opts.help "path to yaml configuration file"

configKeyP :: Opts.Parser String
configKeyP = Opts.strOption $
       Opts.long "config-key"
    <> Opts.value "bench-validation"
    <> Opts.showDefault
    <> Opts.help "configuration key"

blockCountP :: Opts.Parser BlockCount
blockCountP = Opts.option (BlockCount <$> Opts.auto) $
       Opts.long "block-count"
    <> Opts.value 2000
    <> Opts.showDefault
    <> Opts.help "number of blocks to generate"

runsP :: Opts.Parser Int
runsP = Opts.option Opts.auto $
       Opts.long "runs"
    <> Opts.short 'r'
    <> Opts.value 100
    <> Opts.showDefault
    <> Opts.help "number of runs over generated blocks"

applyBlocksP :: Opts.Parser Bool
applyBlocksP = Opts.switch $
       Opts.long "apply"
    <> Opts.short 'a'
    <> Opts.help "apply blocks: runs `verifyAndApplyBlocks` otherwise it runs `verifyBlocksPrefix`"

blockCacheP :: Opts.Parser (Maybe FilePath)
blockCacheP = Opts.optional $ Opts.strOption $
       Opts.long "block-cache"
    <> Opts.help "path to block cache (file where generated blocks are written / read from)"

benchArgsParser :: Opts.Parser BenchArgs
benchArgsParser = BenchArgs
    <$> configPathP
    <*> (T.pack <$> configKeyP)
    <*> blockCountP
    <*> runsP
    <*> applyBlocksP
    <*> blockCacheP

-- | Write generated blocks to a file.
writeBlocks :: FilePath -> OldestFirst NE (Block attr) -> IO ()
writeBlocks path bs = do
    let sbs = serialize' $ fmap (runEitherExtRep . forgetExtRep . EitherExtRep) bs
    BS.writeFile path sbs

-- | Read generated blocks from a file.
readBlocks :: FilePath -> IO (Maybe (OldestFirst NE (Block 'AttrExtRep)))
readBlocks path = do
    sbs <- BS.readFile path
    case decodeFull' decodeWithOffsets labelExtRep sbs
            :: Either
                Text
                (NonEmptyExtRep
                    (EitherExtRep
                        (GenericBlock GenesisBlockchain)
                        (GenericBlock MainBlockchain))
                        'AttrOffsets)
            of
        Left err -> do
            putStrLn err
            return Nothing
        Right (NonEmptyExtRep bs) -> return (Just $ OldestFirst $ fmap (runEitherExtRep . spliceExtRep sbs) bs)

main :: IO ()
main = do
    setupLogging Nothing loggerConfig
    args <- Opts.execParser
        $ Opts.info
            (benchArgsParser <**> Opts.helper)
            (Opts.fullDesc <> Opts.progDesc
                (  "The program generates given ammount of blocks and applies them. "
                )
            )
    startTime <- realTime
    let cfo = defaultConfigurationOptions
            { cfoFilePath = baConfigPath args
            , cfoKey = baConfigKey args
            , cfoSystemStart = Just (Timestamp startTime)
            }
    withCompileInfo $
        withConfigurationsM (LoggerName "verification-bench") Nothing cfo $ \_ !pm ->
            let tp = TestParams
                    { _tpStartTime = Timestamp (convertUnit startTime)
                    , _tpBlockVersionData = genesisBlockVersionData
                    , _tpGenesisInitializer = genesisInitializer
                    }
            in runBlockTestMode tp $ do
                -- initialize databasea
                initNodeDBs pm slotSecurityParam
                bs <- case baBlockCache args of
                    Nothing -> do
                        -- generate blocks and evaluate them to normal form
                        logInfo "Generating blocks"
                        generateBlocks pm (baBlockCount args)
                    Just path -> do
                        fileExists <- liftIO $ doesFileExist path
                        mbs <- if fileExists
                                  then liftIO $ readBlocks path
                                  else return Nothing
                        case mbs of
                            Nothing -> do
                                -- generate blocks and evaluate them to normal form
                                logInfo "Generating blocks"
                                bs <- generateBlocks pm (baBlockCount args)
                                liftIO $ writeBlocks path bs
                                return bs
                            Just bs -> return bs

                satisfySlotCheck bs $ do
                    logInfo "Verifying blocks"
                    let bss = force $ zip ([1..] :: [Int]) $ replicate (baRuns args) bs
                    (times, errs) <- fmap unzip $ forM bss
                        $ \(idx, blocks) -> do
                            logInfo $ sformat ("Pass: "%int) idx
                            (if baApply args
                                then validateAndApply pm blocks
                                else validate pm blocks)

                    let -- drop first three results (if there are more than three results)
                        itimes :: [Float]
                        itimes = map realToFrac (if length times > 3 then drop 3 times else times)
                        -- execution mean time
                        mean :: Float
                        mean = avarage itimes
                        -- standard deviation of the execution time distribution
                        stddev :: Float
                        stddev = sqrt . (\x -> x / realToFrac (length itimes - 1)) . avarage . map ((**2) . (-) mean) $ itimes
                    logInfo $ sformat ("verification and application mean time: "%shown%"μs stddev: "%shown) mean stddev

                    -- print errors
                    let errs' = catMaybes errs
                        errno = length errs'
                    when (errno > 0) $ do
                        logError $ sformat ("Verification/Application errors ("%shown%"):") errno
                        traverse_ (logError . show) errs
    where
        loggerConfig :: LoggerConfig
        loggerConfig = termSeveritiesOutB debugPlus
                <> consoleActionB defaultHandleAction

        avarage :: [Float] -> Float
        avarage as = sum as / realToFrac (length as)

        validate
            :: HasConfigurations
            => ProtocolMagic
            -> OldestFirst NE (Block 'AttrExtRep)
            -> BlockTestMode (Microsecond, Maybe (Either VerifyBlocksException ApplyBlocksException))
        validate pm blocks = do
            verStart <- realTime
            -- omitting current slot for simplicity
            ctx <- getVerifyBlocksContext' Nothing
            res <- (force . either Left (Right . fst)) <$> verifyBlocksPrefix pm ctx blocks
            verEnd <- realTime
            return (verEnd - verStart, either (Just . Left) (const Nothing) res)

        validateAndApply
            :: HasConfigurations
            => ProtocolMagic
            -> OldestFirst NE (Block 'AttrExtRep)
            -> BlockTestMode (Microsecond, Maybe (Either VerifyBlocksException ApplyBlocksException))
        validateAndApply pm blocks = do
            verStart <- realTime
            ctx <- getVerifyBlocksContext' Nothing
            res <- force <$> verifyAndApplyBlocks pm ctx False blocks
            verEnd <- realTime
            case res of
                Left _ -> return ()
                Right (_, blunds)
                    -> whenJust (nonEmptyNewestFirst blunds) (rollbackBlocks pm)
            return (verEnd - verStart, either (Just . Right) (const Nothing) res)

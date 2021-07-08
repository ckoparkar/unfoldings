module Gibbon.Plugin where

import qualified GHC.Plugins as GHC

--------------------------------------------------------------------------------
-- GHC Core-to-Core Plugin
--------------------------------------------------------------------------------

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin { GHC.installCoreToDos = installGibbon
                           -- , GHC.pluginRecompile  = GHC.purePlugin
                           }

installGibbon :: [GHC.CommandLineOption] -> [GHC.CoreToDo] -> GHC.CoreM [GHC.CoreToDo]
installGibbon _ todos = return (gibbonCoreTodo : todos)

gibbonCoreTodo :: GHC.CoreToDo
gibbonCoreTodo = GHC.CoreDoPluginPass "Gibbon" gibbon

gibbon :: GHC.ModGuts -> GHC.CoreM GHC.ModGuts
gibbon mod_guts = do
    GHC.liftIO $ print "[Gibbon] test..."
    hsc_env <- GHC.getHscEnv
    eps <- GHC.liftIO $ GHC.hscEPS hsc_env
    let imports = GHC.nameEnvElts (GHC.eps_PTE eps)
    GHC.putMsg (GHC.ppr (map (\tyt -> case tyt of
                                        GHC.AnId i -> Just (i, GHC.maybeUnfoldingTemplate (GHC.realIdUnfolding i))
                                        _ -> Nothing)
                             imports))
    pure mod_guts

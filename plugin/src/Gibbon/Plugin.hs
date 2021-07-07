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
    pure mod_guts

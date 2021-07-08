module Gibbon.Plugin where

import qualified GHC.Plugins as GHC
import qualified Data.List as L
import           Data.Maybe ( isJust, fromJust )

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

    -- -- (1)
    -- let toplvls = GHC.mg_binds mod_guts
    -- GHC.putMsg (GHC.ppr (map (\i -> let unf = GHC.realIdUnfolding i
    --                                 in if GHC.hasSomeUnfolding unf
    --                                    then (i, Just (GHC.unfoldingTemplate unf))
    --                                    else (i, Nothing))
    --                          (go toplvls)))

    -- (2)
    hsc_env <- GHC.getHscEnv
    eps <- GHC.liftIO $ GHC.hscEPS hsc_env
    -- let pit = GHC.eps_PIT eps
        -- hpt = GHC.hsc_HPT hsc_env
        -- mg = GHC.hsc_mod_graph hsc_env
        -- mss = GHC.mgModSummaries mg
    --     imports = foldr (\ms acc -> GHC.ms_home_imps ms ++ acc) [] mss
    -- GHC.putMsg (GHC.ppr imports)
    -- let modules = GHC.moduleEnvKeys pit
    -- let ifaces = map fromJust $ filter isJust $ map (GHC.lookupIfaceByModule hpt pit) modules
    --     test = last ifaces
    -- GHC.putMsg (GHC.ppr (GHC.mi_decls test))
    let imports = GHC.nameEnvElts (GHC.eps_PTE eps)
    GHC.putMsg (GHC.ppr (map (\tyt -> case tyt of
                                        GHC.AnId i -> Just (i, GHC.maybeUnfoldingTemplate (GHC.realIdUnfolding i))
                                        _ -> Nothing)
                             imports))
    pure mod_guts

  where
    go :: GHC.CoreProgram -> [GHC.Id]
    go toplvls =
        foldr (\bind acc ->
                 case bind of
                     GHC.NonRec _ rhs -> acc ++ (GHC.exprSomeFreeVarsList wantId rhs)
                     GHC.Rec ls -> foldr (\(_,rhs) acc2 -> acc2 ++ (GHC.exprSomeFreeVarsList wantId rhs)) acc ls)
              []
              toplvls

    wantId :: GHC.Id -> Bool
    wantId v =
        let name = GHC.varName v
        in not (GHC.isConLikeId v || notUserDefinedName name || GHC.isWiredInName name)

    notUserDefinedName :: GHC.Name -> Bool
    notUserDefinedName = L.isPrefixOf "$" . GHC.getOccString

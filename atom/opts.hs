module Opts (Flag(..), compileOpts) where

import System.Console.GetOpt
import Data.Maybe (isJust)

type OptsPredicat = [Flag] -> Bool

data Flag =
	Version
	| Verbose
	| Help
	-- Generate a source code.
	| Generate String
	| PrintSched
	deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [
    Option ['h'] ["help"] (NoArg Help) "Show Help",
    Option ['v'] ["verbose"] (NoArg Verbose) "Verbose mode",
    Option ['V'] ["version"] (NoArg Version) "Show version",
	Option ['o'] ["generate"] (ReqArg Generate "File prefix") "Generate C sources",
	Option ['p'] ["printsched"] (NoArg PrintSched) "Print the scheduling hierachy"
  ]

-- Get the list of flags and remaining arguments.
compileOpts :: [String] -> IO ([Flag], [String])
compileOpts argv =
	case getOpt Permute options argv of
		(opts, non_opts, []) -> return (opts, non_opts)
		(_, _, errs) -> ioError(userError (concat errs))

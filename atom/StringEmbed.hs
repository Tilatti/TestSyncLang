module StringEmbed(embedStr, embedStrFile) where
 
import Language.Haskell.TH
import Language.Haskell.TH.Quote

embedStr :: QuasiQuoter 
embedStr = QuasiQuoter { quoteExp = stringE,
                    quotePat = undefined,
                    quoteDec = undefined,
                    quoteType = undefined }

embedStrFile :: QuasiQuoter
embedStrFile = quoteFile embedStr

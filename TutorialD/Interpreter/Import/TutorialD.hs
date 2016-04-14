module TutorialD.Interpreter.Import.TutorialD where
import ProjectM36.Base
import TutorialD.Interpreter.Import.Base
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.DatabaseExpr
import Text.Parsec.String
import Text.Parsec hiding (try)
import ProjectM36.Error
import qualified Data.Text as T
import Control.Exception
--import a file containing TutorialD database context expressions

importTutorialD :: FilePath -> IO (Either RelationalError DatabaseExpr)
importTutorialD pathIn = do
  tutdData <- try (readFile pathIn) :: IO (Either IOError String)
  case tutdData of 
    Left err -> return $ Left (ImportError $ T.pack (show err))
    Right tutdData' -> do 
      let dbexprsErr = parse databaseExprsP "import" tutdData'
      case dbexprsErr of
        Left err2 -> return $ Left (ParseError (T.pack (show err2)))
        Right dbexprs -> return $ Right (MultipleExpr dbexprs)

tutdImportP :: Parser DatabaseContextDataImportOperator
tutdImportP = do
  reserved ":importtutd" 
  path <- quotedString
  whiteSpace
  return $ DatabaseContextDataImportOperator path importTutorialD

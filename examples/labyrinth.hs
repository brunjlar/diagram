import           Diagram.Labyrinth (diagram)
import           Diagrams.Backend.SVG.CmdLine (mainWith)

main :: IO ()
main = mainWith diagram

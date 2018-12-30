import           Diagram.Knot (diagram)
import           Diagrams.Backend.SVG.CmdLine (mainWith)

main :: IO ()
main = mainWith diagram

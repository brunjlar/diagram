import Diagram.Hilbert              (diagram)
import Diagrams.Backend.SVG.CmdLine (mainWith)

main :: IO ()
main = mainWith diagram

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
main = do
    (progname, _) <- getArgsAndInitialize
    createWindow "Hello World"
    displayCallback $= display
    mainLoop

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = map (\k -> (sin(2*pi*k/12), cos(2*pi*k/12), 0.0)) [1..12]

display = do
    clear [ ColorBuffer ]; flush

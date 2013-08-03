#include <SFML/Graphics.hpp>
#include <iostream>

int main()
{
    sf::RenderWindow mainWindow(sf::VideoMode::getDesktopMode(), "sprite.cpp main window");
    /*sf::Image mainSpriteImage;
    if (!mainSpriteImage.loadFromFile("the_kid.jpg")) {
        std::cerr << "ERROR: couldn't load image file" << std::endl;
        exit(1);
    }

    sf::Sprite mainSprite(mainSpriteImage);

    while(mainWindow.isOpen()) {
        sf::Event currentEvent;
        while(mainWindow.getEvent(currentEvent)) {
            if (currentEvent.Type == sf::Event::Closed)
            {
                mainWindow.close();
            }

            if (currentEvent.Type == sf::Event::KeyPressed &&
                currentEvent.Key.Code == sf::Key::W && currentEvent.Key.Control)
            {
                mainWindow.close();
            }
        }

        const sf::Input & mainWindowInput = mainWindow.getInput();

        if (mainWindowInput.isKeyDown(sf::Key::Left)) {
            mainSprite.move(-300.0 * mainWindow.getFrameTime(), 0);
        } else if (mainWindowInput.IsKeyDown(sf::Key::Right)) {
            mainSprite.move(300.0 * mainWindow.getFrameTime(), 0);
        }

        if (mainWindowInput.isKeyDown(sf::Key::Up)) {
            mainSprite.move(0, -300.0 * mainWindow.getFrameTime());
        } else if (mainWindowInput.isKeyDown(sf::Key::Down)) {
            mainSprite.move(0, 300.0 * mainWindow.getFrameTime());
        }

        mainWindow.clear(sf::Color(255,255,255,0));
        mainWindow.draw(mainSprite);
        mainWindow.display();
    }
    */

    return 0;
}

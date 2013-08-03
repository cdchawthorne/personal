#include <SFML/System.hpp>
#include <SFML/Graphics.hpp>
#include <SFML/Audio.hpp>
#include <iostream>
#include <cassert>

int main()
{
    sf::Texture mainSpriteTexture;
    if (!mainSpriteTexture.loadFromFile("the_kid.jpg")) {
        std::cerr << "ERROR: couldn't load texture file" << std::endl;
        return 1;
    }

    sf::Music music;
    if (!music.openFromFile("spike_in_a_rail.ogg"))
    {
        std::cerr << "ERROR: couldn't load music file" << std::endl;
        return 1;
    }

    sf::Sprite mainSprite(mainSpriteTexture);
    sf::Clock clock;
    bool hasFocus = true;
    sf::RenderWindow mainWindow(sf::VideoMode::getFullscreenModes()[0],
                                "sprite.cpp main window");

    music.setLoop(true);
    music.play();
    while(mainWindow.isOpen()) {
        sf::Time elapsed = clock.restart();

        sf::Event event;
        while(mainWindow.pollEvent(event)) {
            if (event.type == sf::Event::Closed)
            {
                mainWindow.close();
            }

            if (event.type == sf::Event::KeyPressed &&
                event.key.code == sf::Keyboard::W && event.key.control)
            {
                mainWindow.close();
            }

            if (event.type == sf::Event::LostFocus)
            {
                hasFocus = false;
            }

            if (event.type == sf::Event::GainedFocus)
            {
                hasFocus = true;
            }
        }

        if (hasFocus)
        {
            if (sf::Keyboard::isKeyPressed(sf::Keyboard::Left)) {
                mainSprite.move(-300.0 * elapsed.asSeconds(), 0);
            } else if (sf::Keyboard::isKeyPressed(sf::Keyboard::Right)) {
                mainSprite.move(300.0 * elapsed.asSeconds(), 0);
            }

            if (sf::Keyboard::isKeyPressed(sf::Keyboard::Up)) {
                mainSprite.move(0, -300.0 * elapsed.asSeconds());
            } else if (sf::Keyboard::isKeyPressed(sf::Keyboard::Down)) {
                mainSprite.move(0, 300.0 * elapsed.asSeconds());
            }
        }

        mainWindow.clear(sf::Color(255,255,255,0));
        mainWindow.draw(mainSprite);
        mainWindow.display();
    }

    return 0;
}

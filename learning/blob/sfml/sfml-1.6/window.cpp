#include <SFML/Window.hpp>
#include <iostream>

int main()
{
    /* Creates a window with nothing in it */
    sf::Window mainWindow(sf::VideoMode::GetMode(0), "window.cpp main window");
    std::cin.get();

    sf::Window deferredWindow;
    std::cin.get();
    deferredWindow.Create(sf::VideoMode::GetMode(0), "window.cpp deferred window");
    std::cin.get();

    while (std::cin.get() != -1) {
        /* Doesn't seem to do anything, for the moment */
        mainWindow.Display();
        deferredWindow.Display();
    }
    return 0;
}

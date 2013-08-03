#include <SFML/Window.hpp>
#include <unistd.h>

int main()
{
    sf::Window main_window(sf::VideoMode::GetMode(0), "events.cpp main window");

    while (main_window.IsOpened()) {
        sf::Event current_event;
        while (main_window.GetEvent(current_event)) {
            if (current_event.Type == sf::Event::Closed)
            {
                main_window.Close();
            }
            
            if (current_event.Type == sf::Event::KeyPressed &&
                current_event.Key.Code == sf::Key::Escape)
            {
                main_window.Close();
            }

            if (current_event.Type == sf::Event::KeyPressed &&
                current_event.Key.Code == sf::Key::W && current_event.Key.Control)
            {
                main_window.Close();
            }
        }

        main_window.Display();
    }
}

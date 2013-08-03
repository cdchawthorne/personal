#include <SFML/Window.hpp>
#include <iostream>
#include <cassert>

int main()
{
    for(int i = sf::VideoMode::GetModesCount() - 1; i >= 0; --i) {
        sf::VideoMode vm = sf::VideoMode::GetMode(i);
        std::cout << "Width: " << vm.Width << std::endl;
        std::cout << "Height: " << vm.Height << std::endl;
        std::cout << "BPP: " << vm.BitsPerPixel << std::endl;
        assert(vm.IsValid());
    }

    //sf::Window main_window(sf::VideoMode(800, 600, 32), "window.cpp window");
    return 0;
}

#include <SFML/Window.hpp>
#include <iostream>
#include <cassert>
#include <vector>

int main()
{
    const std::vector<sf::VideoMode> & modes = sf::VideoMode::getFullscreenModes();
    for(std::vector<sf::VideoMode>::const_iterator it = modes.begin();
        it != modes.end(); ++it)
    {
        const sf::VideoMode & vm = *it;
        std::cout << "Width: " << vm.width << std::endl;
        std::cout << "Height: " << vm.height << std::endl;
        std::cout << "BPP: " << vm.bitsPerPixel << std::endl;
        assert(vm.isValid());
    }

    const sf::VideoMode & vm = sf::VideoMode::getDesktopMode();
    std::cout << "Desktop Mode" << std::endl;
    std::cout << "Width: " << vm.width << std::endl;
    std::cout << "Height: " << vm.height << std::endl;
    std::cout << "BPP: " << vm.bitsPerPixel << std::endl;
    assert(vm.isValid());

    return 0;
}

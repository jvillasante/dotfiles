#include <SFML/Config.hpp>
#include <SFML/Graphics.hpp>

#include <iostream>
#include <ResourcePath.hpp>

int main() {
    std::cout << "SFML Version:" << SFML_VERSION_MAJOR << "." << SFML_VERSION_MINOR << "." << SFML_VERSION_PATCH
              << std::endl;

    auto current_mode = sf::VideoMode::getDesktopMode();
    std::cout << "Current Mode: (" << current_mode.width << 'x' << current_mode.height << ')' << '\n';

    std::vector<sf::VideoMode> modes = sf::VideoMode::getFullscreenModes();
    for (const sf::VideoMode& mode : modes) {
        std::cout << "  Mode: (" << mode.width << 'x' << mode.height << ')' << '\n';
    }

    // sf::RenderWindow window(sf::VideoMode(current_mode.width, current_mode.height), "Bouncing Mushroom");
    sf::RenderWindow window(sf::VideoMode(800, 600), "Bouncing Mushroom");

    window.setFramerateLimit(60);
    window.setMouseCursorVisible(false);

    sf::Texture mushroomTexture;
    mushroomTexture.loadFromFile(resource_path() + "media/textures/Mushroom.png");
    sf::Sprite mushroom(mushroomTexture);
    sf::Vector2u size = mushroomTexture.getSize();
    mushroom.setOrigin(static_cast<float>(size.x) / 2, static_cast<float>(size.y) / 2);
    sf::Vector2f increment(8.0F, 8.0F);
    // mushroom.setColor(sf::Color(255, 0, 0, 150));

    while (window.isOpen()) {
        sf::Event event{};
        while (window.pollEvent(event)) {
            switch (event.type) {
            case sf::Event::Closed:
                window.close();
                break;
            case sf::Event::KeyPressed:
                if (event.key.code == sf::Keyboard::Q) {
                    window.close();
                }
                break;
            default:
                break;
            }
        }

        if ((mushroom.getPosition().x + (size.x / 2) > window.getSize().x && increment.x > 0) ||
            (mushroom.getPosition().x - (size.x / 2) < 0 && increment.x < 0)) {
            increment.x = -increment.x; // Reverse the direction on X axis.
        }

        if ((mushroom.getPosition().y + (size.y / 2) > window.getSize().y && increment.y > 0) ||
            (mushroom.getPosition().y - (size.y / 2) < 0 && increment.y < 0)) {
            increment.y = -increment.y; // Reverse the direction on Y axis.
        }

        mushroom.setPosition(mushroom.getPosition() + increment);

        window.clear(sf::Color(16, 16, 16, 255));
        window.draw(mushroom);
        window.display();
    }

    return 0;
}

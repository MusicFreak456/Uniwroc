#include<SFML/System.hpp>
#include<SFML/Graphics.hpp>
#include<iostream>

#include"Keyboard.hpp"
#include"ControlPanel.hpp"

using namespace std;

int main()
{
    sf::RenderWindow main_window(sf::VideoMode(1760,500),"Creative Process Automator",sf::Style::Close);
    main_window.setFramerateLimit(60);

    sf::Font montserrat_black;
    if(montserrat_black.loadFromFile("fonts/Montserrat-Black.ttf"))
    {
        cout << "Font black loaded" << endl;
    }
    sf::Font montserrat_regular;
    if(montserrat_regular.loadFromFile("fonts/Montserrat-Regular.ttf"))
    {
        cout << "Font regular loaded" << endl;
    }
    sf::Color background(0,32,43);

    Keyboard keyboard;
    keyboard.move_position(0,main_window.getSize().y - WhiteKey::Height);


    Key * hovered_key = nullptr;
    Key * activated_key = keyboard.activate_init_white_key(23);

    ControlPanel ctrl_panel(montserrat_regular,activated_key,&keyboard);

    bool hovering_over_key;

    while(main_window.isOpen())
    {
        sf::Vector2f mouse_coords = main_window.mapPixelToCoords(sf::Mouse::getPosition(main_window));
        ctrl_panel.hovers_detection(mouse_coords);
        Key * new_hovered = keyboard.mouse_over(mouse_coords);
        if(new_hovered!=hovered_key)
        {
            if(hovered_key!=nullptr)hovered_key->reset_color();
            if(new_hovered!=nullptr)
            {
                new_hovered->highlight();
                hovering_over_key=true;
            }
            else hovering_over_key=false;
            hovered_key=new_hovered;
            
        }

        sf::Event event;
        while (main_window.pollEvent(event))
        {
            if(event.type == sf::Event::Closed)
            {
                main_window.close();
            }
            
            if(event.type == sf::Event::MouseButtonPressed)
            {
                if(event.mouseButton.button == sf::Mouse::Left)
                {
                    if(hovering_over_key)
                    {
                        if(activated_key!=nullptr) activated_key->deactivate();
                        hovered_key->activate();
                        ctrl_panel.set_root(hovered_key);
                        activated_key=hovered_key;
                    }
                }
            }
        }

        main_window.clear(background);
        main_window.draw(keyboard);
        main_window.draw(ctrl_panel);
        main_window.display();
    }

    return 0;
}
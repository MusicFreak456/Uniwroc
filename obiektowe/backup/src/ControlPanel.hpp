#ifndef CONTROLPANEL_HPP
#define CONTROLPANEL_HPP
#include"ActiveNoteBracket.hpp"
#include"Keyboard.hpp"
#include"Scale.hpp"
#include<SFML/Graphics.hpp>

class ControlPanel :public sf::Drawable
{
private:
    ActiveNoteBracket root_bracket;
    Scale scale;
    Key * root_note;
    Keyboard * keyboard;

public:
    ControlPanel(sf::Font& ,Key*,Keyboard*);
    void set_root(Key*);
    void hovers_detection(sf::Vector2f);
private:
    void draw(sf::RenderTarget&,sf::RenderStates) const;
};

#endif
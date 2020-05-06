#include"ControlPanel.hpp"

ControlPanel::ControlPanel(sf::Font& font, Key * root_note,Keyboard * keyboard): root_note(root_note), root_bracket(ActiveNoteBracket(font,root_note->get_note())),
keyboard(keyboard), scale(keyboard,root_note,font){}

void ControlPanel::draw(sf::RenderTarget& target,sf::RenderStates states) const
{
    target.draw(this->root_bracket);
    target.draw(this->scale);
}

void ControlPanel::set_root(Key* root_note)
{
    this->root_note = root_note;
    this->root_bracket.set_note(root_note->get_note());
    this->scale.change_root(root_note);
}

void ControlPanel::hovers_detection(sf::Vector2f mouse_coords)
{
    this->scale.hovers_detection(mouse_coords);
}

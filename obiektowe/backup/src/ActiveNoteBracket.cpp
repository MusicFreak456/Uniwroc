#include"ActiveNoteBracket.hpp"

ActiveNoteBracket::ActiveNoteBracket(sf::Font& font,string note)
{
    this->title.setString("Root note");
    this->note.setString(note);
    this->title.setFont(font);
    this->note.setFont(font);
    this->title.setPosition(30,30);
    this->note.setPosition(95,90);
    this->title.setCharacterSize(50);
    this->note.setCharacterSize(130);

    this->rect.setSize(sf::Vector2f(300,300));
    this->rect.setFillColor(sf::Color(255,255,255,10));


}

void ActiveNoteBracket::draw(sf::RenderTarget& target,sf::RenderStates states) const
{
    target.draw(this->rect);
    target.draw(this->note);
    target.draw(this->title);
}

void ActiveNoteBracket::set_note(string note)
{
    this->note.setString(note);
}
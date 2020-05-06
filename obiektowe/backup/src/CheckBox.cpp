#include"CheckBox.hpp"

CheckBox::CheckBox(string title, sf::Font& font)
{
    this->title.setFont(font);
    this->title.setString(title);
    this->title.setCharacterSize(19);
    this->title.setPosition(32,6);
    
    this->border.setSize(sf::Vector2f(150,40));
    this->border.setFillColor(sf::Color(255,255,255,30));

    this->ch_box.setFillColor(sf::Color::White);
    this->ch_box.setSize(sf::Vector2f(10,10));
    this->ch_box.setPosition(10,13);
}

void CheckBox::draw(sf::RenderTarget& target,sf::RenderStates states) const
{
    target.draw(this->border,states);
    target.draw(this->ch_box,states);
    target.draw(this->title,states);
}

void CheckBox::move_position(float x, float y)
{
    this->border.setPosition(this->border.getPosition().x+x , this->border.getPosition().y+y );
    this->ch_box.setPosition(this->ch_box.getPosition().x+x , this->ch_box.getPosition().y+y );
    this->title.setPosition(this->title.getPosition().x+x , this->title.getPosition().y+y );
}

void CheckBox::hovers_detection(sf::Vector2f mouse_coords)
{
    sf::FloatRect bounds = this->border.getGlobalBounds();
    if(bounds.contains(mouse_coords))
    {
        this->border.setFillColor(sf::Color(255,255,255,50));
    }
    else
    {
        this->border.setFillColor(sf::Color(255,255,255,30));
    }
    
}
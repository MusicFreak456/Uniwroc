#include"Key.hpp"

WhiteKey::WhiteKey()
{
    this->active = false;
    this->default_color = sf::Color::White;
    this->setFillColor(this->default_color);
    this->setSize(sf::Vector2f(Width,Height));
}

BlackKey::BlackKey()
{
    this->active = false;
    this->default_color = sf::Color::Black;
    this->setFillColor(this->default_color);
    this->setSize(sf::Vector2f(Width,Height));
}

void WhiteKey::set_note(string note)
{
    this->note = note;
}
string WhiteKey::get_note()
{
    return this->note;
}
void WhiteKey::set_value(float value)
{
    this->value=value;
}
float WhiteKey::get_value()
{
    return this->value;
}

void WhiteKey::reset_color()
{
    this->setFillColor(this->default_color);
}


void WhiteKey::highlight()
{
    this->setFillColor(sf::Color(255,219,77));
}
void BlackKey::highlight()
{
    this->setFillColor(sf::Color(230,184,0));
}

void WhiteKey::activate()
{
    this->active = true;
    this->default_color = sf::Color(0,77,38);
    this->reset_color();
}

void BlackKey::activate()
{
    this->active = true;
    this->default_color = sf::Color(0,77,38);
    this->reset_color();
}

void WhiteKey::deactivate()
{
    this->active =false;
    this->default_color = sf::Color::White;
    this->reset_color();
}

void BlackKey::deactivate()
{
    this->active =false;
    this->default_color = sf::Color::Black;
    this->reset_color();
}

void WhiteKey::in_scale()
{
    this->active =false;
    this->default_color = sf::Color(102,255,179);
    this->reset_color();
}

void BlackKey::in_scale()
{
    this->active =false;
    this->default_color = sf::Color(0,179,89);
    this->reset_color();
}
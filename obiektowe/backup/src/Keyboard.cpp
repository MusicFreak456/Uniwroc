#include"Keyboard.hpp"

const string NOTES[] = {"C","D","E","F","G","A","B","C"}; 

Keyboard::Keyboard(): number_of_white_keys(52), number_of_black_keys(36)
{
    int spacing = 2;
    int indent = ((WhiteKey::Width*2) - BlackKey::Width)/2; 
    int helper = 6;
    int value = 1;

    for(int i=1;i<=number_of_white_keys;i++)
    {
        WhiteKey new_key;
        new_key.set_value(value);
        string note = NOTES[helper-1];
        new_key.set_note(note);
        value++;
        new_key.setPosition(sf::Vector2f((i-1)*WhiteKey::Width + (i-1)* spacing,0));
        white_keys.push_back(new_key);

        if(helper!=7 && helper!=3 && i!=number_of_white_keys)
        {
            BlackKey new_key;
            new_key.setPosition(sf::Vector2f((i-1)*WhiteKey::Width + (i-1)* spacing + indent,0));
            new_key.set_value(value);
            note = note + "#";
            new_key.set_note(note);
            value++;
            black_keys.push_back(new_key);
        }
        helper++;
        if(helper>7)helper=1;
    }

}

void Keyboard::draw(sf::RenderTarget& target,sf::RenderStates states) const
{
    for(const Key & k : white_keys)
    {
        target.draw(k);
    }
    for(const Key & k : black_keys)
    {
        target.draw(k);
    }
}

void Keyboard::move_position(float x, float y)
{
    for(Key & k : white_keys)
    {
        float xcoord = k.getPosition().x;
        float ycoord = k.getPosition().y;
        k.setPosition(sf::Vector2f(xcoord+x,ycoord+y));
    }
    for(Key & k : black_keys)
    {
        float xcoord = k.getPosition().x;
        float ycoord = k.getPosition().y;
        k.setPosition(sf::Vector2f(xcoord+x,ycoord+y));
    }
}

Key* Keyboard::mouse_over(sf::Vector2f mouse__pos)
{
    for(Key & k : black_keys)
    {
        sf::FloatRect bounds = k.getGlobalBounds();
        if(bounds.contains(mouse__pos.x,mouse__pos.y))return &k;
    }
    for(Key & k : white_keys)
    {
        sf::FloatRect bounds = k.getGlobalBounds();
        if(bounds.contains(mouse__pos.x,mouse__pos.y))return &k;

    }
    return nullptr;
}

Key * Keyboard::activate_init_white_key(int i)
{
    this->white_keys[i].activate();
    return &white_keys[i];
}

Key * Keyboard::find_key(float index)
{
    for(Key & k : black_keys)
    {
        if(k.get_value() == index)return &k;
    }
    for(Key & k : white_keys)
    {
        if(k.get_value() == index)return &k;
    }
    return nullptr;
}
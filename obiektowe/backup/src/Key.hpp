#ifndef KEY_HPP
#define KEY_HPP
#include<SFML/Graphics.hpp>

using namespace std;

class Key :public sf::RectangleShape
{
protected:
    string note;
    float value;
    bool active;
public:
    virtual string get_note()=0;
    virtual float get_value()=0;
    virtual void in_scale()=0;
    virtual void highlight()= 0;
    virtual void reset_color()= 0;
    virtual void activate()=0;
    virtual void deactivate()=0;
};

class WhiteKey :public Key
{

public:
    static const int Height = 200;
    static const int Width = 32;
    sf::Color default_color;
public:
    WhiteKey();
    void set_note(string);
    string get_note();
    void set_value(float);
    float get_value();
    void reset_color();
    void highlight();
    void activate();
    void deactivate();
    void in_scale();
};

class BlackKey :public WhiteKey
{
public:
    static const int Height = 120;
    static const int Width = 20;
public:
    BlackKey();
    void highlight();
    void activate();
    void deactivate();
    void in_scale();
};


#endif
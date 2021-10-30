#!/usr/bin/env ruby

class PlainText
  attr_accessor :text
  
  def initialize(text)
    @text = text;
  end

  def code(key)
    to_return = @text
    to_return.length.times {|i| to_return[i] = key[to_return[i]]}
    return CipherText.new(to_return)
  end
end  

class CipherText
  attr_accessor :text

  def initialize(text)
    @text = text
  end

  def decode(key)
    inverted_key = key.invert
    to_return = @text
    to_return.length.times {|i| to_return[i] = inverted_key[to_return[i]]}
    return PlainText.new(to_return)
  end
end



if __FILE__ == $0
  phrase = PlainText.new("ruby")
  key = {
    "a" => "b",
    "b" => "r",
    "c" => "z",
    "d" => "c",
    "e" => "d",
    "f" => "e",
    "g" => "f",
    "h" => "g",
    "i" => "h",
    "j" => "i",
    "k" => "j",
    "l" => "k",
    "m" => "l",
    "n" => "m",
    "o" => "n",
    "p" => "o",
    "q" => "p",
    "r" => "y",
    "s" => "s",
    "t" => "t",
    "u" => "a",
    "v" => "w",
    "w" => "v",
    "x" => "x",
    "y" => "u",
    "z" => "z"
  }
  puts phrase.text
  coded = phrase.code(key)
  puts coded.text
  puts coded.decode(key).text
end
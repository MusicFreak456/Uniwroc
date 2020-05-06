#!/usr/bin/env ruby

class Integer #Fixnum jest przestarzała w obecnej wersji

  def czynniki
    tablica = Array.new

    1.upto(Math.sqrt(self)) do |cand|
      if self%cand==0
        tablica.push(cand)
        tablica.push(self/cand)
      end
    end

    return tablica
  end

  def ack(y)
    if self == 0
      return y + 1
    elsif y == 0
      return (self - 1).ack(1)
    else
      return (self - 1).ack(self.ack(y-1))
    end
  end

  def doskonala
    czynniki = self.czynniki
    czynniki.delete(self)
    return czynniki.sum == self
  end

  def slownie
    slownik = {
      1 => "jeden",
      2 => "dwa",
      3 => "trzy",
      4 => "cztery",
      5 => "pięć",
      6 => "sześć",
      7 => "siedem",
      8 => "osiem",
      9 => "dziewięć"
    }
    tabilca_cyfr = self.digits
    tabilca_cyfr = tabilca_cyfr.reverse
    tabilca_cyfr = tabilca_cyfr.map{|cyfra| slownik[cyfra]}
    return tabilca_cyfr.join(" ")
  end

end


if __FILE__ == $0
  puts 6.czynniki
  puts 27.doskonala
  puts 28.doskonala
  puts 2.ack(1)
  puts 42.slownie

end
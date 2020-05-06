#!/usr/bin/env ruby

class Function
    def initialize(block)
        @fun = block
    end

    def value(x)
        @fun.call(x)
    end

    def derivative(x)
        delta = 0.0001
        y1 = self.value(x)
        y2 = self.value(x+delta)

        return (y2 - y1) / delta

    end

    def integral(a,b)
        field = 0
        delta = (b-a)/1000.0
        beg = a

        1000.times do
            field += (self.value(beg) + self.value(beg + delta)) * delta /2
            beg += delta
        end

        return field
    end

    def root (a, b, e) 

        if b - a <= e
            if self.value(b) * self.value(a) < 0
                return [a]
            else
                return []
            end
        end

        return self.root(a , a + (a-b).abs / 2.0, e).concat( self.root(a + (a-b).abs / 2.0, b , e) )

    end

end

if __FILE__ == $0
    function1 = Function.new(Proc.new{ |x| x*x})
    puts "f(x) = x^2"
    puts "f(2) = #{function1.value(2)}"
    puts "integral from 0 to 3 = #{function1.integral(0,3)}"

    function2 = Function.new(Proc.new{ |x| Math.sin(x)})
    puts "g(x) = sin(x)"
    puts "Integral from 0 to pi = #{function2.integral(0,Math::PI)}" 
    puts "Derivative in 1 = #{function2.derivative(1)}"

    function3 = Function.new(Proc.new{ |x| x+2})
    puts "h(x) = x-2"
    puts "roots"
    puts function3.root(-3,3,0.001)

    function4 = Function.new(Proc.new{ |x| x*x*x - x*x -4})
    puts "i(x) = x^3 - x^2 - 4"
    puts "roots"
    puts function4.root(-3,3,0.001)

    function5 = Function.new(Proc.new{ |x| x*x -2})
    puts "h(x) = x^2 - 2"
    puts "roots"
    puts function5.root(-2,2,0.001)


end
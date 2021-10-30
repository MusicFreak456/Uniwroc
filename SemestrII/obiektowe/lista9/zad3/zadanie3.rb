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

    def drawPBM(a,b,filename)
        width = 1000
        height = 1000
        step = (a.abs + b.abs) / width
        file = File.open(filename,"w")
        file.write("P1\n")
        file.write("#{width} #{height}\n")

        table = Array.new(height) {Array.new(width," 0 ")}

  
        height.times do |y|
            pos = a
            width.times do |x|
                if(y == height/2 || pos.abs<0.000001)
                    table[y][x]="1"
                end
                pos+=step
            end
        end

        pixval = 1.0/step

        pos=a
        width.times do |x|
           heightpos = -self.value(pos)*pixval + height/2
           if heightpos >= 0 && heightpos <= height-1
            table[heightpos][x] = "1"
           end
           pos+=step
        end

        height.times do |y|
            width.times do |x|
                file.write(table[y][x])
            end
            file.write("\n")
        end
    
        file.close
    end

end

if __FILE__ == $0
    function1 = Function.new(Proc.new{ |x| x+3}) 
    function1.drawPBM(-5.0,5.0, "lin.pbm")

    function2 = Function.new(Proc.new{ |x| x*x}) 
    function2.drawPBM(-5.0,5.0, "quad.pbm")

    function3 = Function.new(Proc.new{ |x| Math.sin(x)}) 
    function3.drawPBM(-5.0,5.0, "sin.pbm")

    function4 = Function.new(Proc.new{ |x| Math.tan(x)}) 
    function4.drawPBM(-5.0,5.0, "tan.pbm")
end
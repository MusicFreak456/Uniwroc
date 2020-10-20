#!/usr/bin/env ruby

class Element
    @value
    @next
    attr_accessor :value
    attr_accessor :next

    def initialize(value)
        @value = value
        @next = nil
    end
end

class Collection
    @element
    @length

    def initialize
        @element = Element.new(nil)
        @length = 0
    end

    def to_s
        curr_elem = @element.next
        display_str = ""
        while curr_elem != nil
            display_str = display_str + curr_elem.value.to_s + " "
            curr_elem = curr_elem.next
        end

        return display_str
    end

    def length
        return @length
    end

    def add(value)
        curr_elem = @element
        @length +=1

        while curr_elem.next != nil
            curr_elem = curr_elem.next
        end

        curr_elem.next = Element.new(value)
    end

    def get(index)
        curr_elem = @element

        (index + 1).times do
            curr_elem = curr_elem.next
        end

        return curr_elem.value
    end

    def swap(i,j)
        ielem = get_elem(i)
        jelem = get_elem(j)

        ivalue = ielem.value
        ielem.value = jelem.value
        jelem.value = ivalue
    end

    private

    def get_elem(index)
        curr_elem = @element

        (index + 1).times do
            curr_elem = curr_elem.next
        end

        return curr_elem
    end
end

class Sort
    def self.bubble_sort(collection)
        length = collection.length
        0.upto(length-1) do |x|
            0.upto(length-2-x) do |y|
                if collection.get(y) > collection.get(y+1)
                    collection.swap(y,y+1)
                end
            end
        end
    end

    def self.selection_sort(collection)
        length = collection.length
        
        0.upto(length-1) do |x|
            min = collection.get(x)
            min_index = x
            x.upto(length-1) do |y|
                get_val = collection.get(y)
                if get_val < min
                    min = get_val
                    min_index = y
                end
            end
            if x != min_index
                collection.swap(x,min_index)
            end
        end
    end
end

if __FILE__ == $0
    collection1 = Collection.new
    collection1.add(1)
    collection1.add(5)
    collection1.add(3)
    collection1.add(2)
    puts collection1.get(0)
    puts collection1.get(1)
    puts collection1.to_s
    Sort::bubble_sort(collection1)
    puts collection1.to_s

    collection2 = Collection.new
    collection2.add(1)
    collection2.add(5)
    collection2.add(3)
    collection2.add(2)
    Sort::selection_sort(collection2)
    puts collection2.to_s

end
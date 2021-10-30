#!/usr/bin/env ruby

class Element
    @value
    @next
    @prev
    attr_accessor :value
    attr_accessor :next
    attr_accessor :prev

    def initialize(value)
        @value = value
        @next = nil
        @prev = nil
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
            if curr_elem.next.value > value

                new_elem = Element.new(value)

                new_elem.next = curr_elem.next
                new_elem.prev = curr_elem

                curr_elem.next.prev = new_elem
                curr_elem.next = new_elem
                return
            end 
            curr_elem = curr_elem.next
        end

        curr_elem.next = Element.new(value)
        curr_elem.next.prev = curr_elem
    end

    def get(index)
        curr_elem = @element

        (index + 1).times do
            curr_elem = curr_elem.next
        end

        return curr_elem.value
    end
end

class Search
    def self.bin_search(collection,value)
        return bin_search_aux(collection,value,0,collection.length-1)
    end

    def self.interpol_search(collection,value)
        return interpol_search_aux(collection,value,0,collection.length-1)
    end

    private

    def self.bin_search_aux(collection,value,start_range,end_range)
        mid = (start_range+end_range)/2
        mid_value = collection.get(mid)

        if(mid_value == value)
            return mid
        elsif(start_range==end_range)
            return -1
        end

        if(mid_value > value)
            return bin_search_aux(collection,value,start_range,mid)
        else
            return bin_search_aux(collection,value,mid+1,end_range)
        end
    end

    def self.interpol_search_aux(collection,value,start_range,end_range)
        start_val = collection.get(start_range)
        end_val = collection.get(end_range)
        if start_range <= end_range && value >= start_val && value <= end_val

            pos = (start_range + ((end_range - start_range) / (end_val - start_val).to_f) * (value - start_val)).to_i
            pos_val = collection.get(pos)

            if pos_val == value
                return pos
            elsif pos_val < value
                return interpol_search_aux(collection,value,pos+1,end_range)
            else
                return interpol_search_aux(collection,value,start_range,pos-1,x)
            end
        end

        return -1
    end
end

if __FILE__ == $0
    collection = Collection.new
    collection.add(4)
    collection.add(2)
    collection.add(42)
    collection.add(5)
    puts collection.to_s
    puts Search.bin_search(collection,4)
    puts Search.interpol_search(collection,4)
    puts Search.bin_search(collection,2)
    puts Search.interpol_search(collection,2)
    puts Search.bin_search(collection,42)
    puts Search.interpol_search(collection,42)
    puts Search.bin_search(collection,33)
    puts Search.interpol_search(collection,33)
end
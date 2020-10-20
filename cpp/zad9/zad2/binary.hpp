#include<iostream>
#include<fstream>
#include<cstring>

using std::string;
using std::cout;
using std::endl;

namespace binf
{
    class InputStream
    {
    private:
        std::ifstream input_file;
    public:
        InputStream(string);

        template<typename T>
        InputStream& operator>>(T& value)
        {

            char byte_char = 0;
            uint8_t byte = 0;
            uint64_t holder = 0;
            int size_of_value = sizeof(value);

            for (int i = 1; i <= size_of_value; i++)
            {
                if(!input_file.good())throw std::ios_base::failure("Failed to read");
                input_file.get(byte_char);
                byte = byte_char;
                holder = holder | ((uint64_t)byte << ((size_of_value-i) * 8));
            }

            memcpy(&value, &holder, sizeof(holder));
            

            return *this;
        }

        void close();
        bool eof();

        ~InputStream();  
    };

    class OutputStream
    {
    private:
        std::ofstream output_file;
    public:
        OutputStream(string);

        template<typename T> 
        OutputStream& operator<<(const T& value)
        {
            
            uint8_t byte = 0;
            uint64_t holder = 0;
            int size_of_value = sizeof(value);

            memcpy(&holder,&value,sizeof(value));

            for(int i=1; i<=size_of_value; i++)
            {
                if(!output_file.good()) throw std::ios_base::failure("Failed to write");
                byte = 0;
                byte = (holder >> ((size_of_value - i) * 8)) | byte;
                output_file << (uint8_t)byte;
            }

            return *this;
        }

        void close();
        bool eof();

        ~OutputStream();
    };
}
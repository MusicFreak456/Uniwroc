#include"binary.hpp"



binf::InputStream::InputStream(string filename)
{
    this->input_file.open(filename, std::ios::binary);
}

binf::InputStream::~InputStream()
{
    this->input_file.close();
}

binf::OutputStream::OutputStream(string filename)
{
    this->output_file.open(filename, std::ios::binary);
}

binf::OutputStream::~OutputStream()
{
    this->output_file.close();
}

void binf::InputStream::close()
{
    this->input_file.close();
}

void binf::OutputStream::close()
{
    this->output_file.close();
}

bool binf::OutputStream ::eof()
{
    return this->output_file.eof();
}

bool binf::InputStream::eof()
{
    return this->input_file.eof();
}

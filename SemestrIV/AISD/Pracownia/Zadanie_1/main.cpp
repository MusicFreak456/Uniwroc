#include<cstdio>
#include<vector>
#include<algorithm>
#define ullint unsigned long long int

// Ten sposób liczenia zapalonych bitów był prezentowany
// np. na zajęciach z Architektur Systemów Komputerowych
int popcount_64(ullint x) {
    x =    (0x5555555555555555 & x) + ((x >> 1) & 0x5555555555555555);
    x =    (0x3333333333333333 & x) + ((x >> 2) & 0x3333333333333333);
    x =    (0x0F0F0F0F0F0F0F0F & x) + ((x >> 4) & 0x0F0F0F0F0F0F0F0F);
    x =    (0x00FF00FF00FF00FF & x) + ((x >> 8) & 0x00FF00FF00FF00FF);
    x =    (0x0000FFFF0000FFFF & x) + ((x >> 16)& 0x0000FFFF0000FFFF);
    return (0x00000000FFFFFFFF & x) + ((x >> 32)& 0x00000000FFFFFFFF);
}

bool comparison_function(
    std::pair<ullint,ullint> &a, 
    std::pair<ullint,ullint> &b){
    return (a.first < b.first);
}

std::vector<std::pair<ullint,ullint>> read_example(){
    std::vector<std::pair<ullint,ullint>> list_of_pairs;
    ullint number_of_lines;

    (void)!scanf("%llu", &number_of_lines);

    for(; number_of_lines >= 1; number_of_lines--){
        ullint a, b;
        (void)!scanf("%llu %llu", &a, &b );
        list_of_pairs.push_back(std::make_pair(a,b));
    }
    return list_of_pairs;
}

void translate_pair_to_odd_base(std::pair<ullint,ullint> &in){
    
    while (! (in.first % 2))
    {
       in.first = in.first >> 1;
       in.second = in.second << 1; 
    }
}

ullint solve(
    std::vector<std::pair<ullint,ullint>> &list_of_pairs){

    ullint result = 0;

    for(std::pair<ullint,ullint> &pair: list_of_pairs) {
        translate_pair_to_odd_base(pair);
    }

    std::sort(list_of_pairs.begin(), list_of_pairs.end(), comparison_function);

    ullint vec_size = list_of_pairs.size();
    ullint sum_of_odd = 0;

    for(ullint i = 0; i < vec_size; i++) {
        sum_of_odd += list_of_pairs[i].second;
        if(i == vec_size-1){
            result += popcount_64(sum_of_odd);
        }
        else if(list_of_pairs[i].first != list_of_pairs[i+1].first){
            result += popcount_64(sum_of_odd);
            sum_of_odd = 0;
        }
    } 

    return result;
}


int main()
{
    std::vector<std::pair<ullint,ullint>> example = read_example();
    printf("%llu", solve(example));

    return 0;
}
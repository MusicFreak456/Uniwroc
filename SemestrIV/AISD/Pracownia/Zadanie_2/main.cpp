#include<cstdio>
#include<vector>
#include<algorithm>

std::pair<std::vector<int>,int> read_example(){
    std::vector<int> example;
    int size;
    int sum=0;
    (void)!scanf("%d", &size);

    for(; size >= 1; size--){
        int x;
        (void)!scanf("%d", &x);
        example.push_back(x);
        sum+=x;
    }

    return std::make_pair(std::move(example),sum);
}

void fill_first_row(std::vector<int> &row, int elem){
    row[elem] = elem;
}

int append_to_lesser(int elem, int diff, const int prev){
    if(prev != 0){
        return std::max((prev - diff) + elem,prev);
    }
    return prev;
}

int append_to_bigger(int elem, const int prev){
    if(prev != 0){
        return prev + elem;
    }
    return prev;
}

void fill_row(
    const std::vector<int> &previous_row, 
    std::vector<int>       &current_row,
    int elem,
    int guard
){
    int sum = previous_row.size() - 1;
    for(int i=0; i <= guard; i++){
        int value = previous_row[i];
        int candidate = 0;
        int min_diff = elem - i;
        int min_diff_abs = abs(min_diff);
        int max_diff = elem + i;

        if(i == elem && elem > value) value = elem;
        if(min_diff > 0){
            candidate = append_to_lesser(elem, min_diff, previous_row[min_diff]);
            if(candidate > value) value = candidate;
        }
        else if(min_diff_abs <= sum){
            candidate = append_to_bigger(elem, previous_row[min_diff_abs]);
            if(candidate > value) value = candidate;
        }
        if(max_diff <= sum && i!=0){
            candidate = append_to_lesser(elem, max_diff, previous_row[max_diff]);
            if(candidate > value) value = candidate;
        }
        
        current_row[i] = value;
    }
}

void solve(std::vector<int> &example, int sum){
    std::sort(example.begin(), example.end());
    int example_size = example.size();
    std::vector<int> row1(sum+1,0);
    std::vector<int> row2(sum+1,0);

    fill_first_row(row1,example[0]);
    int guard = example[0];
    for(int i = 1; i<example_size; i++){
        guard += example[i];
        if(i%2){
            fill_row(row1, row2, example[i], guard);
        }
        else{
            fill_row(row2, row1, example[i], guard);
        }
    }
    
    std::vector<int> result = std::move(row2);
    if(example_size%2){
        result = std::move(row1);
    }

    if(result[0]){
        printf("TAK\n");
        printf("%d", result[0]);
    }
    else{
        for(int i=1; i<=sum; i++){
            int current_h = result[i];
            if( current_h && current_h - i){
                printf("NIE\n");
                printf("%d",i);
                break;
            }
        }
    }
}

int main()
{
    std::pair<std::vector<int>,int> example(std::move(read_example()));
    solve(example.first, example.second);

    return 0;
}
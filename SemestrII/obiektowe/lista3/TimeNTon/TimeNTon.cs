using System;
using Const;

namespace TimeNTon
{
    sealed public class Timenton
    {
        public string id;
        static int count;

        Timenton(){}
        static Timenton[] instance = new Timenton[Consts.N];
        
        public static Timenton Instance()
        {
            DateTime CurrentTime = DateTime.Now;
            if(CurrentTime.Hour >= 16 && CurrentTime.Hour <= 18) // 
            {
                
                if(count < Consts.N)
                {
                    instance[count]= new Timenton();
                    return instance[count++];
                }
                else
                {
                    int index = count % Consts.N;
                    count++;
                    return instance[index];
                }

            }
            else if(instance[0] == null)
            {
                count =1;
                instance[0] = new Timenton();
                return instance[0];
            }
            else 
            {
                return instance[0];
            }
        }
    }
}

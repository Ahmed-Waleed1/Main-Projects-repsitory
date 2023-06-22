#include <stdio.h>
#include <stdlib.h>
#include <string.h>


char Students[1000],poi_line[100], StudentsCode[1000], *token4, *token1, *token3, no_student[1000], CoursesExams[500], CourseCode[500], operation[100], line[100],single_line[100],*token2;
int stundet_counter, NO_students, search, i, j, q, k, F, max, max1, M, c, GATE, OPERATION;
int b = 1, m = 1, column = 1, location = 1;
int array2[50][6], minimum[50], array[50];
int** array1;
size_t str_length;


int main()
{

//making if condition in order to enter this part of code one time only when the program start.
if(GATE == 0)
{

    //creating txt files needed for store the data from program to text file.
    FILE * pointer_f1_13 = fopen("Exams data.txt","w");
    fclose(pointer_f1_13);
    FILE * pointer_f1_11 = fopen("Exams data arrange.txt","w");
    fclose(pointer_f1_11);
    FILE * pointer_f1_12 = fopen("CONTAINER.txt","w");
    fclose(pointer_f1_12);

    FILE * pointer_f_13 = fopen("Students data.txt","w");
    fclose(pointer_f_13);
    FILE * pointer_f_11 = fopen("Students data arrange.txt","w");
    fclose(pointer_f_11);





    //in this part of code the program take the input from user and change its form so it be more easier
    //to stored it in 2d array to use this data in functions

    //TAKE input from user in the form of string and PUT it in two text file.
    FILE * pointer_f_3 = fopen("Exams data.txt","w");
    FILE * pointer_f1_3 = fopen("students data.txt","w");

    scanf("%s",CoursesExams);
    scanf("\n%s",Students);

    fprintf(pointer_f_3,"%s",CoursesExams);
    fprintf(pointer_f1_3,"%s",Students);

    fclose(pointer_f_3);
    fclose(pointer_f1_3);




    //it put the data from text file to 1d array so we can change the forum of data by
    //delete unnecessary parts the from the two strings
    FILE * pointer_f_4 = fopen("Exams data.txt","r");
    FILE * pointer_f1_4 = fopen("Students data.txt","r");

    fgets(CourseCode,500,pointer_f_4);
    fgets(StudentsCode,1000,pointer_f1_4);

    fclose(pointer_f_4);
    fclose(pointer_f1_4);




    //by using function strtok we will put the data without {CoursesExams=[]} and {;} in new text files
    FILE * pointer_f_5 = fopen("Exams data arrange.txt","w");
    token3 = strtok(CourseCode,"CoursesExams=[];");
    while(token3 != NULL)
        {
            fprintf(pointer_f_5,"%s\n",token3);
            token3 = strtok(NULL,"CoursesExams=[];");
        }
    fclose(pointer_f_5);

    FILE * pointer_f1_5 = fopen("Students data arrange.txt","w");
    token4 = strtok(StudentsCode,"Students=[];");
    while(token4 != NULL)
        {
            fprintf(pointer_f1_5,"%s\n",token4);
            token4 = strtok(NULL,"Students=[];");
        }
    fclose(pointer_f1_5);
    //now the input is on text file and the arrange of it changed now every student is in row.



    //COUNTER :
    //making counter with name { NO_students } to count the number of students in the text file.
    FILE * pointer_f_6 = fopen("Students data arrange.txt","r");
    while(!feof(pointer_f_6))
        {
            fgets(poi_line,100,pointer_f_6);
            NO_students = NO_students + 1;
        }
    fclose(pointer_f_6);
    NO_students = NO_students - 1;



    //2D ARRAY :
    //making 2d array that contain in every row student and in each in element
    //in every row there will be in the first position
    //and the rest of position will be course that
    //the student enrolled in, this 2d array doesn't contain trash values int this elements
    FILE * pointer_f_7 = fopen("Students data arrange.txt","r");
    //loop for the NO_STUDENTS WHICH CONTAIN THE NUMBEROF students.
    for(j = 0; j < NO_students; j++)
    {
        //take the data from text file to measure it length
        while(!feof(pointer_f_7))
            {
                    fgets(single_line,100,pointer_f_7);
                    if(single_line != '\n')
                        {
                            str_length = strlen(single_line);
                            str_length = str_length - 1;
                            array[j] = str_length;
                            break;
                        }


            }


    }

    fclose(pointer_f_7);

        //change the length of string with numbers that indicate the number of elements in every array.
        for(q = 0; q < NO_students; q++)
            {

                if(array[q] == 10)
                    {
                        array[q] = 2;
                    }
                else if(array[q] == 14)
                    {
                        array[q] = 3;
                    }
                else if(array[q] == 18)
                    {
                        array[q] = 4;
                    }
                else if(array[q] == 22)
                    {
                        array[q] = 5;
                    }
                else if(array[q] == 26)
                    {
                        array[q] = 6;
                    }

            }


    //create and open text file to delete the { , } and replace it with space.
    FILE * pointer_f_8 = fopen("Students data arrange.txt","r");
    FILE * pointer_f_9 = fopen("CONTAINER.txt","w");
    while(!feof(pointer_f_8))
        {
            fgets(line,100,pointer_f_8);
            token2 = strtok(line,",");
            while(token2 != NULL)
                {
                    fprintf(pointer_f_9,"%s ",token2);
                    token2 = strtok(NULL,",");
                }

        }
    fclose(pointer_f_8);
    fclose(pointer_f_9);


        //Allocate memory for the array1 ,and make it determine
        //the the number of elements in each array in the 2d array
        array1 = malloc(NO_students * sizeof(*array1)); // Assuming `NO_students` is the number of rows

        for(i = 0; i < NO_students; i++)
        {
            array1[i] = malloc(column * sizeof(**array1));
            column++;
        }


    //Then, read data from the file into the array1.
    FILE * pointer_f_10 = fopen("CONTAINER.txt", "r");

        for(i = 0; i < NO_students; i++)
            {

                    if(k < b)
                    {
                        max = array[k];
                        k++;
                        b++;

                        for(column = 0; column < max; column++)
                            {
                                fscanf(pointer_f_10, "%d", &array1[i][column]);
                            }

                    }

            }
    fclose(pointer_f_10);


        for(i = 0; i < NO_students; i++)
            {


                    if(F < m)
                    {
                        max1 = array[F];
                        F++;
                        m++;
                    }


            }

    //increase the GATE value by one to prevent going to the steps above again.
    GATE = GATE + 1;
}



//take the name of operation.

scanf("%s", &operation);



//convert the operation name with integer to unable to put it in switch function.
if(strcmp(operation, "Number_Students") == 0)
    {
        OPERATION = 1;
    }
    else if(strcmp(operation, "Number_Halls") == 0)
    {
        OPERATION = 2;
    }
    else if(strcmp(operation, "Student_ID_Min") == 0)
    {
        OPERATION = 3;
    }
    else if(strcmp(operation, "Students_Dropped_ID") == 0)
    {
        OPERATION = 4;
    }
    else if(strcmp(operation, "Exams_Period_InDays") == 0)
    {
        OPERATION = 5;
    }
    else if(strcmp(operation, "Course_Students") == 0)
    {
        OPERATION = 6;
    }
    else if(strcmp(operation, "List_Course_Students_More") == 0)
    {
        OPERATION = 7;
    }
    else if(strcmp(operation, "List_Student_Courses_Less") == 0)
    {
        OPERATION = 8;
    }
    else if(strcmp(operation, "List_Hall_Students") == 0)
    {
        OPERATION = 9;
    }
    else if(strcmp(operation, "List_Hall_Students_InAnyday") == 0)
    {
        OPERATION = 10;
    }
    else if(strcmp(operation, "Quit") == 0)
    {
        OPERATION = 11;
    }

    //put operations in switch function to make it easy t
    switch(OPERATION)
    {
        case 1:;//Number_Students

            //operation 1:
            //create new array to put the data in it to run it in the operation without destroy or change the original
            //input data of the user and increment the { stundet_counter } to measure the number of students
            FILE * pointer_f_15 = fopen("students data.txt","r");
            fgets(no_student,1000,pointer_f_15);
            fclose(pointer_f_15);

            token1 = strtok(no_student,";");
            while(token1 != NULL)
                {
                    stundet_counter = stundet_counter+1;
                    token1 = strtok(NULL, ";");
                }
            printf("\n%d\n",stundet_counter);

            break;



        case 2 ://Number_Halls

            break;



        case 3 ://Student_ID_Min

            //operation 3:

            for(i = 0 ; i < NO_students; i++)
                {

                    minimum[i] = array1[i][0];

                }


            for (c = 1; c < NO_students; c++)
                {

                    if (array1[c][0] > minimum[c])
                        {
                            minimum[c] = array2[c][0];
                            location = c + 1;
                        }
                }
            printf("\n%d\n", minimum[location]);


            break;

        case 4 ://Students_Dropped_ID

            break;



        case 5 ://Exams_Period_InDays

            break;



        case 6 ://Course_Students //CourseCode

            //operation 6:

            scanf("%d",&search);

            for(i = 0 ; i < NO_students; i++)
                {

                    for(j = 0; j < column ;j++)
                        {
                            if(array1[i][j] == search)
                                {
                                    printf("\n%d\n",array1[i][0]);

                                    break;
                                }

                        }

            }

            break;




        case 7 ://List_Course_Students_More​  // n

            break;




        case 8 ://List_Student_Courses_Less // n

            //operation 8:

            scanf("%d",&M);//M --->  no of id

            M = M + 1;
            for(c = 0 ; c < NO_students; c++)
                {

                    if(array[c] < M)//M+1
                        {
                            printf("%d\n",array1[c][0]);

                            break;
                        }

                }

            break;




        case 9 ://List_Hall_Students  //HallName,Date

            break;




        case 10 ://List_Hall_Students_InAnyday  //HallName

            break;



        //print thanks and then return 0; to end the program.
        case 11 ://Quit

            printf("\nThanks!\n");

            return 0;
            break;

        default:
            printf("\nThanks\n");

    }
    main();




    return 0;
}

/*
Team members;

-	Ahmed Wael Abdrahman Abdelmonem …………………. 19104953
-	Ahmed Waleed Fahmy Ali ………………………………………. 19100838
-	Amr Nabil Sayed Fahmy …………………………………………. 19100592
-	Mohammed Ahmed Abdelrahim Elemary ………………. 19104825
-	Youssef Mohammed Abdelhamed Omara ………………. 19100935
*/
//the program tested and worked with these two string samples:
//CoursesExams=[101,28/4/2016,A;201,3/5/2016,A;110,5/5/2016,C;103,5/5/2016,A;120,6/5/2016,D;132,7/5/2016,B]
//Students=[191001,201;191002,201,101;191003,479,205,201,207,111;191004,500,654,123,207]

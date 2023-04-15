MODULE mg
    IMPLICIT NONE

    ! This module compute the minority game without memory

    ! Parameters:
    INTEGER, PARAMETER :: n = 101, & ! number of agents
                          s = 2, & ! number of strategies
                          t = 100 ! number of iteractions
    
    ! memory size
    INTEGER :: m  

    ! Variance
    REAL*8 :: variance

    ! The strategy per agent and their score
    INTEGER, ALLOCATABLE :: agent_strategy(:,:,:,:)

    ! Attendance vector containing the value (1) and index (2) for each
    !   agent in each time
    INTEGER :: attendance(n,t,2)



    CONTAINS

    SUBROUTINE allocate_arrays()
        ALLOCATE(agent_strategy(n,s,2**m,2))
    END SUBROUTINE

    SUBROUTINE deallocate_arrays()
        DEALLOCATE(agent_strategy)
    END SUBROUTINE


    SUBROUTINE make_strategy()
        INTEGER :: i, j, k, D
        
        D = 2**m

        DO i = 1, N
            DO j = 1, s
                DO k = 1, D
                    agent_strategy(i,j,k,1) = INT(RAND() + 0.5)
                END DO
                agent_strategy(i,j,:,2) = 0
            END DO
        END DO
    END SUBROUTINE 

    ! Each agent choose their strategy
    SUBROUTINE play(time_step)
        INTEGER :: i, best_strategy_s_index, mu, D               
        INTEGER, INTENT(IN) :: time_step
        INTEGER, DIMENSION(1) :: temp_array

        D = 2**m

        DO i = 1,N
            ! Choose the best strategy. Here we used just the
            !   first outcome since all have the same s.
            temp_array = MAXLOC(agent_strategy(i,:,1,2))
            best_strategy_s_index = temp_array(1)

            ! Define a condition for the first play
            IF (agent_strategy(i,best_strategy_s_index,1,2) == 0) THEN
                best_strategy_s_index = 1 + INT(RAND()*s)
            END IF

            ! Define an randon number between 1 and D
            mu = 1 + INT(RAND()*D)


            ! Define the random outcome for the best strategy 
            !   and store it.
            attendance(i,time_step,1) = agent_strategy(i,best_strategy_s_index,mu, 1)
            ! Store the index of the best strategy to be
            !   punctuated later
            attendance(i,time_step,2) = best_strategy_s_index
        END DO
    END SUBROUTINE

    ! Define who wins the game in an specific time_step
    SUBROUTINE winner(winner_output,time_step)
        INTEGER :: counter_0, counter_1,temp_array_0(N),temp_array_1(N)
        INTEGER, INTENT(IN) :: time_step
        INTEGER, INTENT(OUT) :: winner_output

        counter_0 = COUNT(attendance(:,time_step,1) == 0)

        counter_1 = COUNT(attendance(:,time_step,1) == 1)

        IF (counter_0 < counter_1) THEN
            winner_output = 0
        ELSE IF (counter_0 > counter_1) THEN
            winner_output = 1
        ELSE
            winner_output = INT(RAND() + 0.5)
        END IF
    END SUBROUTINE

    ! Subroutine that punctuate the minority strateggy
    SUBROUTINE punctuate(time_step)
        INTEGER :: i, winner_temp,best_strategy_s_index
        INTEGER, INTENT(IN) :: time_step

        CALL winner(winner_temp,time_step)
        DO i = 1,n
            IF (attendance(i,time_step,1) == winner_temp) THEN
                ! Add a point to the best strategy
                best_strategy_s_index = attendance(i,time_step,2)
                agent_strategy(i,best_strategy_s_index,:,2) &
                = agent_strategy(i,best_strategy_s_index,:,2) + 1
            END IF
        ENDDO
    END SUBROUTINE

    ! Execute the simulation
    SUBROUTINE main()
        INTEGER :: i
        REAL :: variance_temp, start, finish,start_2,finish_2

        CALL allocate_arrays()

        CALL make_strategy()

        DO i = 1,t
            CALL play(i)
            CALL punctuate(i)
            variance = variance + (SUM(attendance(:,i,1)) - n/2)**2
        END DO
        CALL deallocate_arrays()
        variance = variance / t
        Variance = SQRT(variance)
    END SUBROUTINE
END MODULE

PROGRAM mg_program
USE mg
IMPLICIT NONE

INTEGER :: i,j
REAL :: variance_final(20)


OPEN(10, file='variance.txt', status='replace', &
action='write')

DO i = 1,13
    m = i
    variance_final(i) = 0
    DO j = 1,100
        CALL main()
        variance_final(i) = variance_final(i) + variance 
    END DO
    variance_final(i) = variance_final(i) / 100
    WRITE(10,*) i, variance_final(i)
    WRITE(*,*) 'm ', i, 'done'
END DO

CLOSE(10)

END program

   program main

!!    SWAT_HRU_Variable_Change
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This code is to change the three HRU variables of
!!      - SLSOIL: Slope length for lateral subsurface flow [m]
!!      - Depth_IMP: Depth to impervious layer in soil profile [mm]
!!      - R2ADJ: Curve number retention parameter adjustment factor
!!               to adjust surface runoff for flat slopes
!!    based on the wetness class of the HRU.
!!    These variables are needed to perform SWAT-Wil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ Note ~ ~ ~
!!    File identification numbers are starting from 1000
!!    Format identification numbers are starting from 4000
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ Input files ~ ~ ~
!!    Input file needed are:
!!      - fig.fig file: it has the name of all subbasins
!!      - *.sub: it contains the name of hru and soil files located in the subbasin
!!      - *.hru: it cintains the hru related parameters
!!      - *.sol: it contains the soil related parameters
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ....        .......        ............
!!    aaa         |NA            |comment flag
!!    chmfile     |NA            |Compiled HTML file names (.chm)
!!    content     |NA            |content of hru file
!!    depimp      |mm            |Depth to impervious layer in soil profile
!!    eof         |none          |end of file flag (=-1 if eof, else =0)
!!    figfile     |NA            |name of watershed configuration file (.fig)
!!    gwfile      |NA            |Groundwater file names (.gw)
!!    hrufile     |NA            |HRU file names (.hru)
!!    icd         |none          |routing command code (.fig)
!!    iht         |none          |hydrograph storage location number (.fig)
!!    inm1        |none          |1st routing command variable (.fig)
!!    inm2        |none          |2nd routing command variable (.fig)
!!    inm3        |none          |3rd routing command variable (.fig)
!!                               |if icd=1, inm3=subbasin #
!!    istr1       |NA            |wetness class in string format
!!    mgtfile     |NA            |management file names (.mgt)
!!    mline       |NA            |maximum number of lines in hru file
!!    mweti       |NA            |maximum number of wetness classes
!!    numhru      |NA            |total number of HRUs in a subbasin
!!    r2adj       |NA            |Curve number retention parameter adjustment factor
!!                               |to adjust surface runoff for flat slopes
!!    slsoil      |m             |Slope length for lateral subsurface flow
!!    snam        |NA            |name of the HRU's soil type
!!    solfile     |NA            |Soil file names (.sol)
!!    subfile     |NA            |Subbasin file names (.sub)
!!    titldum     |NA            |description line
!!    weti        |NA            |wetness class
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    caps
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    Defining and allocating variables***********************************************************************

           character (len=13) :: subfile, hrufile, solfile, mgtfile, chmfile, gwfile
           character (len=80) :: titldum
           character (len=16) :: snam
           character (len=1) ::  aaa
           character (len=2) :: istr1
           integer :: mweti, numhru
           integer :: icd, inm1, inm2, inm3, iht, mline
           integer :: eof
           real, dimension (:), allocatable :: weti, depimp, slsoil, r2adj
           character(len=300), dimension (:), allocatable :: content

!!    Initialize Variables************************************************************************************
            subfile = ""
            aaa = ""
            eof = 0
            mhru1 = 1
            mweti = 0
            icd = 1
            iht = 0
            inm1 = 0
            inm2 = 0
            inm3 = 0

!!    Open the file which has the variables to be used for changes************************************************
      open (1000,file="variables.txt")
      read (1000,4000) titldum
      write (*,*) titldum
      ! read the total number of wetness classes
      read (1000,4001)mweti
      write (*,*) 'Muximum number of wetness classes:', mweti

      allocate (weti(mweti))
      allocate (depimp(mweti))
      allocate (slsoil(mweti))
      allocate (r2adj(mweti))
      weti = 0
      depimp = 0
      slsoil = 0
      r2adj = 0

      read (1000,4000) titldum
      do i=1,mweti
      read (1000,*,iostat=eof) weti(i), depimp(i),slsoil(i), r2adj(i)
      if (eof < 0) exit  ! this line privent the code to stop if reached to the end of the file
      end do
      close (1000)
      !write (*,*)depimp(:),slsoil(:), r2adj(:)

!!    Open fig file which has the name of the subbasins***********************************************************
      open (1001,file="fig.fig")
      read (1001,4300) aaa, icd, iht, inm1, inm2, inm3 ! reading first line of the fig file to start the do while
      do while (icd == 1)
            subfile = ""
            numhru = 0
            !! calculate total number of HRUs in watershed
       read (1001,4200) subfile
       write (*,*) subfile

       ! Open Subbasin file which has the total number of HRUs and their names++++++++++++++++++++++++++++++++
            call caps(subfile)
            open (1002,file=subfile)

            do j = 1,52 ! skip 52 lines
              read (1002,4000) titldum
            end do

            read (1002,*) numhru ! read the number of hrus
            write (*,*)numhru

            do j = 1,8 ! skip 8 lines
              read (1002,4000) titldum
            end do

            ! Make Changes to HRU files-----------------------------------------------------------------------
            do j = 62, 62 + numhru
                hrufile = ""
                solfile = ""
                snam = ""
                ! read name of files
                read (1002,4400,iostat=eof) hrufile, mgtfile, solfile, chmfile, gwfile
                if (eof < 0) exit

                ! Open Soil File to get soil name.............................................................

                call caps(solfile)
                open (1003,file=solfile)
                read (1003,4000) titldum
                read (1003,4100) snam
                !write (*,*) snam
                close (1003)

                ! open HRU files  to change...................................................................

                call caps(hrufile)
                open (1004,file=hrufile)
                ! copying HRU file contents^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                mline = 46
                allocate (content(mline))
                content (1:mline) = ""
                do i=1,mline
                read (1004,4000,iostat=eof) content(i)
                if (eof < 0) exit
                end do
                close (1004)

                ! Applying changes ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                open (1005,file=hrufile)!, access="sequential", form="formatted", action="readwrite" )

                !write (*,*) hrufile
                ! compare the edc based on soil name
                do k = 1, mweti
                    !write(*,*) "This is wetness"
                    !write (*,*) k
                    write (istr1,"(I2.2)") k
                    if (snam(4:5) == istr1) then
                        !write(*,*) "This is EDC"
                        !write (*,*) snam(4:5)
                        do jj = 1,7 ! Skip first 7 lines
                            read (1005,4000) titldum
                        end do

                        ! change SLSOIL: Slope length for lateral subsurface flow [m]
                        write (1005,4500) slsoil (k) , '| SLSOIL : Slope length for lateral subsurface flow [m]'
                        write (1005,4000) content (9:mline)
                        !write (*,*) "slsoil is done"

                        rewind (1005)
                        do jjj = 1,23 ! Skip first 23 lines
                            read (1005,4000) titldum
                        end do
                        ! change Depth_IMP: Depth to impervious layer in soil profile [mm]
                        write (1005,4500) depimp (k), '| DEP_IMP : Depth to impervious layer in soil profile [mm]'
                        write (1005,4000)content  (25:mline)
                        !write (*,*) "Depth_IMP is done"

                        rewind (1005)
                        do jjjj = 1,44 ! Skip first 44 lines
                            read (1005,4000) titldum
                        end do
                        ! change R2ADJ: Curve number retention parameter adjustment factor to adjust surface runoff for flat slopes
                        write (1005,4500) r2adj (k), '| R2ADJ: Curve number retention parameter adjustment factor &
                                                    &to adjust surface runoff for flat slopes'
                        write (1005,4000)content (46:mline)
                        !write (*,*) "r2adj is done"

                     end if
                end do
                close (1005)
                DEALLOCATE (content)
                !..........................................................................................................
            end do
            close (1002)
            !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        !end select
        !end if
       read (1001,4300) a, icd, iht, inm1, inm2, inm3 ! reading the icd for the next step
      end do

      close (1001)

write (*,*) "Operation Completed"
!!    Format Specifications ***********************************************************************************************
 4000 format (a)
 4001 format (17x,i2)
 4100 format (12x,a16)
 4200 format (10x,a13)
 4300 format (a1,9x,5i6)
 4400 format (8a13,i6)
 4500 format (9x, f7.1,4x,a)

end program

MODULE input_netcdf_module

    ! nf_open_check( char(*) inputfile_name, int option, int file_id)
    ! nf_close_check( char(*) inputfile_name, int file_id)
    ! nf_create_check( char(*) outputfile_name, int option, int file_id)
    ! nf_def_dim_check( int file_id, char(*) dim_name, int(*) dim_size, int dim_id)
    ! nf_def_var_check( int file_id, char(*) var_name, int var_type, int var_dim, int(*) var_size, int var_id)
    ! nf_enddef_check( char(*) outputfile_name, int file_id)
    ! nf_put_var_int_check( int file_id, char(*) var_name, int var_id, int(*) input_var)
    ! nf_put_var_real_check( int file_id, char(*) var_name, int var_id, real(*) input_var)
    ! nf_put_var_double_check( int file_id, char(*) var_name, int var_id, double(*) input_var)
    ! nf_inq_dimid_check( int file_id, char(*) dim_name, int dim_id)
    ! nf_inq_varid_check( int file_id, char(*) var_name, int var_id)
    ! nf_get_vara_int_check( int file_id, int var_id, char(*) var_name, int(*) NC_START, int(*) NC_COUNT, int(*) output_var)
    ! nf_get_vara_real_check( int file_id, int var_id, char(*) var_name, int(*) NC_START, int(*) NC_COUNT, real(*) output_var)
    ! nf_get_vara_double_check( int file_id, int var_id, char(*) var_name, int(*) NC_START, int(*) NC_COUNT, double(*) output_var)
    ! unstag_onedim_double( double(*) data_in, double(*) data_out, int nx, int ny, int nz, int tgt_dim)
    ! unstag_time_int(data_in, data_out, nx, ny, nz, nt, tgt_dim)
    ! unstag_time_real(data_in, data_out, nx, ny, nz, nt, tgt_dim)
 
    implicit none
    include "netcdf.inc"

    CONTAINS

    SUBROUTINE nf_open_check(inputfile_name, option, file_id)
        CHARACTER(*), intent(in) :: inputfile_name
        INTEGER, intent(in)      :: option
        INTEGER, intent(inout)   :: file_id
        INTEGER                  :: check

        check = nf_open(inputfile_name, option, file_id)
        if (check.ne.0) then
            WRITE(*,*) 'nf_open ERROR ', check
            WRITE(*,*) '              ', inputfile_name
            STOP
        end if
    END SUBROUTINE nf_open_check
    
    SUBROUTINE nf_close_check(inputfile_name, file_id)
        CHARACTER(*), intent(in) :: inputfile_name
        INTEGER, intent(inout)   :: file_id
        INTEGER                  :: check

        check = nf_close(file_id)
        if (check.ne.0) then
            WRITE(*,*) 'nf_close ERROR ', check
            WRITE(*,*) '               ', inputfile_name
            STOP
        end if
    END SUBROUTINE nf_close_check

    SUBROUTINE nf_create_check(outputfile_name, option, file_id)
        CHARACTER(*), intent(in) :: outputfile_name
        INTEGER, intent(in)      :: option
        INTEGER, intent(inout)   :: file_id
        INTEGER                  :: check

        check = nf_create(outputfile_name, option, file_id)
        if (check.ne.0) then
            WRITE(*,*) 'nf_create ERROR ', check
            WRITE(*,*) '                ', outputfile_name
            STOP
        end if
    END SUBROUTINE nf_create_check

    SUBROUTINE nf_def_dim_check(file_id, dim_name, dim_size, dim_id)
        CHARACTER(*), intent(in) :: dim_name
        INTEGER, intent(in)      :: file_id, dim_size
        INTEGER, intent(inout)   :: dim_id
        INTEGER                  :: check

        check = nf_def_dim(file_id, dim_name, dim_size, dim_id)
        if (check.ne.0) then
            WRITE(*,*) 'nf_def_dim ERROR ', check
            WRITE(*,*) '                 ', dim_name
            STOP
        end if
    END SUBROUTINE nf_def_dim_check

    SUBROUTINE nf_def_var_check(file_id, var_name, var_type, var_dim, var_size, var_id)
        INTEGER, intent(in)      :: file_id, var_type, var_dim
        INTEGER, intent(in)      :: var_size(var_dim)
        CHARACTER(*), intent(in) :: var_name
        INTEGER, intent(inout)   :: var_id
        INTEGER                  :: check

        check = nf_def_var(file_id, var_name, var_type, var_dim, var_size, var_id)
        if (check.ne.0) then
            WRITE(*,*) 'nf_def_var ERROR ', check
            WRITE(*,*) '                 ', var_name
            STOP
        end if
    END SUBROUTINE nf_def_var_check
    
    SUBROUTINE nf_enddef_check(outputfile_name, file_id)
        CHARACTER(*), intent(in) :: outputfile_name
        INTEGER, intent(inout)   :: file_id
        INTEGER                  :: check

        check = nf_enddef(file_id)
        if (check.ne.0) then
            WRITE(*,*) 'nf_close ERROR ', check
            WRITE(*,*) '               ', outputfile_name
            STOP
        end if
    END SUBROUTINE nf_enddef_check
    
    SUBROUTINE nf_put_var_int_check(file_id, var_name, var_id, input_var)
        INTEGER, intent(in)      :: file_id, var_id
        CHARACTER(*), intent(in) :: var_name
        INTEGER, intent(in)      :: input_var(*)
        INTEGER                  :: check

        check = nf_put_var_int(file_id, var_id, input_var)
        if (check.ne.0) then
            WRITE(*,*) 'nf_put_var_int ERROR ', check
            WRITE(*,*) '                     ', var_name
            STOP
        end if
    END SUBROUTINE nf_put_var_int_check
    
    SUBROUTINE nf_put_var_real_check(file_id, var_name, var_id, input_var)
        INTEGER, intent(in)      :: file_id, var_id
        CHARACTER(*), intent(in) :: var_name
        REAL, intent(in)         :: input_var(*)
        INTEGER                  :: check

        check = nf_put_var_real(file_id, var_id, input_var)
        if (check.ne.0) then
            WRITE(*,*) 'nf_put_var_real ERROR ', check
            WRITE(*,*) '                      ', var_name
            STOP
        end if
    END SUBROUTINE nf_put_var_real_check
    
    SUBROUTINE nf_put_var_double_check(file_id, var_name, var_id, input_var)
        INTEGER, intent(in)      :: file_id, var_id
        CHARACTER(*), intent(in) :: var_name
        REAL(kind=8), intent(in) :: input_var(*)
        INTEGER                  :: check

        check = nf_put_var_double(file_id, var_id, input_var)
        if (check.ne.0) then
            WRITE(*,*) 'nf_put_var_double ERROR ', check
            WRITE(*,*) '                        ', var_name
            STOP
        end if
    END SUBROUTINE nf_put_var_double_check

    SUBROUTINE nf_inq_dimid_check(file_id, dim_name, dim_id)
        INTEGER, intent(in)      :: file_id
        CHARACTER(*), intent(in) :: dim_name
        INTEGER, intent(inout)   :: dim_id
        INTEGER                  :: check
        
        check = nf_inq_dimid(file_id, dim_name, dim_id)
        if (check.ne.0) then
            WRITE(*,*) 'nf_inq_dimid ERROR ', check
            WRITE(*,*) '                   ', dim_name
            STOP
        end if
    END SUBROUTINE nf_inq_dimid_check
    
    SUBROUTINE nf_inq_varid_check(file_id, var_name, var_id)
        INTEGER, intent(in)      :: file_id
        CHARACTER(*), intent(in) :: var_name
        INTEGER, intent(inout)   :: var_id
        INTEGER                  :: check
        
        check = nf_inq_varid(file_id, var_name, var_id)
        if (check.ne.0) then
            WRITE(*,*) 'nf_inq_varid ERROR ', check
            WRITE(*,*) '                   ', var_name
            STOP
        end if
    END SUBROUTINE nf_inq_varid_check

    SUBROUTINE nf_get_vara_double_check(file_id, var_id, var_name, NC_START, NC_COUNT, output_var)
        INTEGER, intent(in)           :: file_id, var_id, NC_START(:), NC_COUNT(:)
        CHARACTER(*), intent(in)      :: var_name
        REAL(kind=8), intent(inout)   :: output_var(*)
        INTEGER                       :: check
        
        check = nf_get_vara_double(file_id, var_id, NC_START, NC_COUNT, output_var)
        if (check.ne.0) then
            WRITE(*,*) 'nf_get_vara_double ERROR ', check
            WRITE(*,*) '                         ', var_name
            STOP
        end if
    END SUBROUTINE nf_get_vara_double_check
    
    SUBROUTINE nf_get_vara_int_check(file_id, var_id, var_name, NC_START, NC_COUNT, output_var)
        INTEGER, intent(in)           :: file_id, var_id, NC_START(:), NC_COUNT(:)
        CHARACTER(*), intent(in)      :: var_name
        INTEGER, intent(inout)        :: output_var(*)
        INTEGER                       :: check
        
        check = nf_get_vara_int(file_id, var_id, NC_START, NC_COUNT, output_var)
        if (check.ne.0) then
            WRITE(*,*) 'nf_get_vara_int ERROR ', check
            WRITE(*,*) '                      ', var_name
            STOP
        end if
    END SUBROUTINE nf_get_vara_int_check

    SUBROUTINE nf_get_vara_real_check(file_id, var_id, var_name, NC_START, NC_COUNT, output_var)
        INTEGER, intent(in)           :: file_id, var_id, NC_START(:), NC_COUNT(:)
        CHARACTER(*), intent(in)      :: var_name
        REAL, intent(inout)            :: output_var(*)
        INTEGER                       :: check
        
        check = nf_get_vara_real(file_id, var_id, NC_START, NC_COUNT, output_var)
        if (check.ne.0) then
            WRITE(*,*) 'nf_get_vara_real ERROR ', check
            WRITE(*,*) '                       ', var_name
            STOP
        end if
    END SUBROUTINE nf_get_vara_real_check

    SUBROUTINE unstag_onedim_double(data_in, data_out, nx, ny, nz, tgt_dim)
        INTEGER, intent(in) :: nx, ny, nz, tgt_dim
        REAL(kind=8), intent(in)  :: data_in(:,:,:)
        REAL(kind=8), intent(out) :: data_out(:,:,:)
        
        INTEGER :: xCtr, yCtr, zCtr

        if (tgt_dim.eq.3) then
            do zCtr = 1, nz-1
                data_out(:,:,zCtr) = (data_in(:,:,zCtr)+data_in(:,:,zCtr+1)) / 2.d0
            end do
        end if
    END SUBROUTINE unstag_onedim_double

    SUBROUTINE unstag_onedim_real(data_in, data_out, nx, ny, nz, tgt_dim)
        INTEGER, intent(in) :: nx, ny, nz, tgt_dim
        REAL, intent(in)  :: data_in(:,:,:)
        REAL, intent(out) :: data_out(:,:,:)
        
        INTEGER :: xCtr, yCtr, zCtr

        if (tgt_dim.eq.1) then
            do xCtr = 1, nx-1
                data_out(xCtr,:,:) = (data_in(xCtr,:,:)+data_in(xCtr+1,:,:)) / 2.d0
            end do
        end if
        if (tgt_dim.eq.2) then
            do yCtr = 1, ny-1
                data_out(:,yCtr,:) = (data_in(:,yCtr,:)+data_in(:,yCtr+1,:)) / 2.d0
            end do
        end if
        if (tgt_dim.eq.3) then
            do zCtr = 1, nz-1
                data_out(:,:,zCtr) = (data_in(:,:,zCtr)+data_in(:,:,zCtr+1)) / 2.d0
            end do
        end if
    END SUBROUTINE unstag_onedim_real
    
    SUBROUTINE unstag_time_int(data_in, data_out, nx, ny, nz, nt, tgt_dim)
        INTEGER, intent(in) :: nx, ny, nz, nt, tgt_dim
        INTEGER, intent(in)  :: data_in(:,:,:,:)
        INTEGER, intent(out) :: data_out(:,:,:,:)
        
        INTEGER :: tCtr

        if (tgt_dim.eq.1) then
            do tCtr = 1, nx-1
                data_out(tCtr,:,:,:) = (data_in(tCtr,:,:,:)+data_in(tCtr+1,:,:,:)) / 2.d0
            end do
        end if
        if (tgt_dim.eq.2) then
            do tCtr = 1, ny-1
                data_out(:,tCtr,:,:) = (data_in(:,tCtr,:,:)+data_in(:,tCtr+1,:,:)) / 2.d0
            end do
        end if
        if (tgt_dim.eq.3) then
            do tCtr = 1, nz-1
                data_out(:,:,tCtr,:) = (data_in(:,:,tCtr,:)+data_in(:,:,tCtr+1,:)) / 2.d0
            end do
        end if
        if (tgt_dim.eq.4) then
            do tCtr = 1, nt-1
                data_out(:,:,:,tCtr) = (data_in(:,:,:,tCtr)+data_in(:,:,:,tCtr+1)) / 2.d0
            end do
        end if
    END SUBROUTINE unstag_time_int
    
    SUBROUTINE unstag_time_real(data_in, data_out, nx, ny, nz, nt, tgt_dim)
        INTEGER, intent(in) :: nx, ny, nz, nt, tgt_dim
        REAL, intent(in)  :: data_in(:,:,:,:)
        REAL, intent(out) :: data_out(:,:,:,:)
        
        INTEGER :: tCtr

        if (tgt_dim.eq.1) then
            do tCtr = 1, nx-1
                data_out(tCtr,:,:,:) = (data_in(tCtr,:,:,:)+data_in(tCtr+1,:,:,:)) / 2.d0
            end do
        end if
        if (tgt_dim.eq.2) then
            do tCtr = 1, ny-1
                data_out(:,tCtr,:,:) = (data_in(:,tCtr,:,:)+data_in(:,tCtr+1,:,:)) / 2.d0
            end do
        end if
        if (tgt_dim.eq.3) then
            do tCtr = 1, nz-1
                data_out(:,:,tCtr,:) = (data_in(:,:,tCtr,:)+data_in(:,:,tCtr+1,:)) / 2.d0
            end do
        end if
        if (tgt_dim.eq.4) then
            do tCtr = 1, nt-1
                data_out(:,:,:,tCtr) = (data_in(:,:,:,tCtr)+data_in(:,:,:,tCtr+1)) / 2.d0
            end do
        end if
    END SUBROUTINE unstag_time_real

END MODULE input_netcdf_module

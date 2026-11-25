#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

!------------------------------------------------------------------------------
! Description:
!   Defines jules_rivers_props namelist and contains related routines
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in HYDROLOGY
!------------------------------------------------------------------------------

MODULE jules_rivers_props_mod

USE io_constants, ONLY: max_sdf_name_len, max_file_name_len

USE jules_rivers_mod, ONLY: land_dx, land_dy, nx_land_grid, ny_land_grid,      &
   rivers_length, rivers_regrid, x1_land_grid, y1_land_grid

USE logging_mod, ONLY: log_fatal, log_info

USE missing_data_mod, ONLY: imdi, rmdi

USE model_interface_mod, ONLY: identifier_len

USE um_types, ONLY: real_jlslsm

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Definition of the jules_rivers_props namelist
!-----------------------------------------------------------------------------

! Scalars in namelist.
INTEGER, PARAMETER :: nvars_max = 17
  ! The maximum possible number of ancillary variables that are read in.

INTEGER :: nvars
    ! The number of variables in this section

INTEGER :: nx_rivers, ny_rivers
    ! The sizes of the x and y dimensions of the river routing input grid.

LOGICAL ::                                                                     &
  l_find_grid = .FALSE.,                                                       &
    ! Switch controlling how characteristics of the land and rivers grids are
    ! determined.
    ! F = use namelist values for the variables nx_land_grid, ny_land_grid,
    !     x1_land_grid and y1_land_grid, which describe the land grid. Note
    !     that in this case rivers_regrid=F requires nx_land_grid and
    !     ny_land_grid only so as to ensure historical results are maintained.
    ! T = calculate details of the land grid from the known coordinates of
    !     land points. This also triggers differences in how the river domain
    !     is set up, including better treatment of the cases in which the
    !     resolutions of the land and river grids differ and/or land points
    !     (e.g. from a regional domain) straddle the longitudinal edges of a
    !     global river input grid.
  l_use_area = .FALSE.
    ! Switch (used with RFM) to use a drainage area ancillary field to identify
    ! river points.
    ! T means use ancillary data for area.
    ! F means no ancillary data provided. Area will be set above threshold so
    ! that all points will be identified as rivers.

LOGICAL ::                                                                     &
  l_ignore_ancil_rivers_check = .FALSE.
    ! Switch to check river routing & river number ancillary for compatibility.
    ! The current Rose stem test ancils are not compatible as the land sea
    ! mask is different. Setting this to true maintains bit comparability      &
    ! protects the calculation of the coupling field needed for the LFRic
    ! coupled miniapp.

! Information about the files to use
LOGICAL :: read_list = .FALSE.
    ! T - The given file contains a list of file names
    ! F - Ancillary data should be read directly from the given file template
CHARACTER(LEN=max_file_name_len) :: FILE
    ! The file to use for variables that need to be filled from a file.
    ! Exact usage depends on read_list:
    ! read_list = T
    ! Contains a list of files; cannot contain variable name templating.
    ! Coordinates variables are read from coordinate_file.
    ! read_list = F
    ! The name of the file (or variable name template).
    ! Coordinate variables are also read from this file, assuming file does not
    ! include variable-name templating.

CHARACTER(LEN=max_file_name_len) :: coordinate_file = ''
    ! The name of the file given for reading coordinate
    ! values from. This is only used if the ancillary file
    ! name includes templating (i.e. ancillary variables will
    ! come from multiple files), in which case this variable
    ! is used to provide clarity as to where the coordinates
    ! are read from.

CHARACTER(LEN=max_sdf_name_len) :: x_dim_name, y_dim_name
    ! The names of the x and y dimensions.

! Arrays in namelist.
REAL(KIND=real_jlslsm) :: const_val(nvars_max)
    ! The constant value to use for each variable if
    ! use_file = F for that variable

LOGICAL :: use_file(nvars_max)
    !   T - the variable uses the file
    !   F - the variable is set using a constant value

LOGICAL :: is_climatology(nvars_max)
    !   T - the file is a 12-month climatology
    !   F - the file is for the correct time

CHARACTER(LEN=max_sdf_name_len) :: tpl_name(nvars_max)
    ! The name to substitute in a template for each variable
CHARACTER(LEN=identifier_len) :: var(nvars_max)
    ! The variable identifiers of the variables
CHARACTER(LEN=max_sdf_name_len) :: var_name(nvars_max)
    ! The name of each variable in the file

NAMELIST  /jules_rivers_props/                                                 &
   x_dim_name, y_dim_name, nx_rivers, ny_rivers, FILE, coordinate_file,        &
   nvars, var, use_file, is_climatology, read_list, var_name, tpl_name,        &
   const_val,                                                                  &
   land_dx, land_dy, l_find_grid, l_use_area, nx_land_grid, ny_land_grid,      &
   rivers_length, rivers_regrid, x1_land_grid, y1_land_grid

!-----------------------------------------------------------------------------
! Scalers not in namelist
!-----------------------------------------------------------------------------
LOGICAL ::                                                                     &
  l_calc_rivers_length = .FALSE.
    ! Flag indicating if rivers_length will be calculated.

!------------------------------------------------------------------------------
! Array variables.
!------------------------------------------------------------------------------
INTEGER, ALLOCATABLE :: direction_grid(:,:)
  ! Integer version of rivers_dir (the river routing direction index).
  ! We use the REAL version for i.o (JULES cannot read an integer ancillary
  ! field) but thereafter the integer version is easier to use.

INTEGER, ALLOCATABLE :: grid_riv_pt_number(:,:)
  ! Map full river grid to river points. For each location on the grid, this
  ! is the river point number.

INTEGER, ALLOCATABLE :: nextx_grid(:,:)
  ! x index of the next downstream point.

INTEGER, ALLOCATABLE :: nexty_grid(:,:)
  ! y index of the next downstream point.

INTEGER, ALLOCATABLE :: river_mask(:,:)
  ! Indicates type of each point.
  ! Values are sea, land_off_domain or land_in_domain.

CHARACTER(LEN=max_file_name_len) :: file_name_coords
    ! The name of the file to be used to read coordinate values from.

!------------------------------------------------------------------------------
! Declarations of ancillary variables. These variables are only used
! during initialisation of standalone runs.
!------------------------------------------------------------------------------
! Grid index variables.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE :: river_nextx_grid(:,:)
   ! x index of next downstream point.
REAL(KIND=real_jlslsm), ALLOCATABLE :: river_nexty_grid(:,:)
   ! y index of next downstream point.

! Arrays defined on 1d river points vectors
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_xgrid(:)
                          ! Coordinate values for x-dimension of rivers grid
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_ygrid(:)
                          ! Coordinate values for y-dimension of rivers grid

!------------------------------------------------------------------------------
! Ancillary arrays defined on full 2D rivers grid.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_seq(:,:)
                          ! TRIP - River routing pathway sequence
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_dir(:,:)
                          ! River routing direction index
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_dra(:,:)
                          ! RFM - Catchment area draining to a grid cell
                          !    (no. of grid cells)
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lat2d(:,:)
                          ! Full 2D latitude field
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_lon2d(:,:)
                          ! Full 2D longitude field
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_outflow_number(:,:)
                          ! Maps ocean outflow gridboxes to the river they
                          !    belong to (on rivers grid)
                          ! Used for coupling to ocean model
REAL(KIND=real_jlslsm), ALLOCATABLE :: rivers_storage(:,:)
                          ! Initial river storage (kg) values on 2D grid
                          ! Required to allow River storage to be initialised
                          ! from an ancillary on the 2d Rivers grid.
                          ! The ancillary can either be for a single time or
                          ! a 12-month climatology
REAL(KIND=real_jlslsm), ALLOCATABLE :: land_fraction_2d(:,:)
                          ! Land fraction on global 2D grid

!------------------------------------------------------------------------------
! Ancillary arrays for CaMa-Flood, defined on 2D rivers grid.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  channel_depth_grid(:,:),                                                     &
     ! River channel depth (to bankfull level) (m).
  channel_width_grid(:,:),                                                     &
     ! River channel width (at bankfull level) (m).
  mean_sea_level_grid(:,:),                                                    &
     ! Mean sea level elevation at river mouth, relative to top of river
     ! channel (m). Only values at river mouths are used.
  river_distance_grid(:,:),                                                    &
     ! Distance to next downstream gridpoint (m).
  river_elevation_grid(:,:),                                                   &
     ! Elevation of top of river channel (m).
  river_length_grid(:,:),                                                      &
     ! Length of river reach (m).
  river_manning_grid(:,:)
     ! Manning roughness coefficient for river channel (1).

!------------------------------------------------------------------------------
! Ancillary arrays for reservoirs, defined on 2D rivers grid.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  res_capacity_grid(:,:),                                                      &
     ! Storage capacity of reservoirs (kg).
  res_catch_grid(:,:),                                                         &
    ! Catchment area of reservoirs (m2).
  res_year_grid(:,:),                                                          &
    ! Initialisation year of reservoirs.
  res_critical_grid(:,:),                                                      &
    ! Critical storage of reservoir (kg).
  res_flood_grid(:,:),                                                         &
    ! Flood storage of reservoir (kg).
  res_emergency_grid(:,:),                                                     &
    ! Emergency storage of reservoir (kg).
  res_normal_release_grid(:,:),                                                &
    ! Normal release rate of reservoir (kg s-1).
  res_flood_release_grid(:,:)
    ! Flood release rate of reservoir (kg s-1).

!------------------------------------------------------------------------------
! Ancillary arrays for minor reservoirs, defined on 2D rivers grid.
!------------------------------------------------------------------------------
REAL(KIND=real_jlslsm), ALLOCATABLE ::                                         &
  minor_res_capacity_grid(:,:),                                                &
     ! Storage capacity of minor reservoirs (kg).
  minor_res_frac_grid(:,:)
    ! Catchment area of minor reservoirs as fraction of area.

CONTAINS

!##############################################################################

SUBROUTINE check_jules_rivers_props()

USE jules_rivers_mod, ONLY: nx_rivers_in=>nx_rivers, ny_rivers_in=>ny_rivers,  &
                            i_river_vn, land_dx, land_dy, l_init_storage,      &
                            l_minor_reservoirs, l_reservoirs,                  &
                            l_riv_overbank, nx_land_grid, ny_land_grid,        &
                            x1_land_grid, y1_land_grid, rivers_length,         &
                            l_outflow_per_river, rivers_camaflood,             &
                            rivers_regrid, rivers_rfm

USE model_grid_mod, ONLY: l_coord_latlon

USE overbank_inundation_mod, ONLY: overbank_simple, overbank_simple_rosgen,    &
                                   overbank_model

USE string_utils_mod, ONLY: to_string

USE templating_mod, ONLY: tpl_has_var_name

USE jules_print_mgr, ONLY: jules_message

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'CHECK_JULES_RIVERS_PROPS'

! Local variables
INTEGER :: i ! Loop counter

INTEGER, PARAMETER ::                                                          &
   nvar_values = 19         ! Number of recognised identifiers
CHARACTER(LEN=identifier_len) ::                                               &
   var_values(nvar_values) ! Names of recognised identifiers

!end of header
!------------------------------------------------------------------------------

!----------------------------------------------------------------------------
! Pass the values of nx_rivers and ny_rivers that were read from
! jules_rivers_props to the variables in jules_rivers_mod.
! Note that these are only needed by the master task.
!----------------------------------------------------------------------------
nx_rivers_in = nx_rivers
ny_rivers_in = ny_rivers

! Check that the requested identifiers are recognised.
var_values(:) = ['area                   ',                                    &
                 'direction              ',                                    &
                 'sequence               ',                                    &
                 'latitude_2d            ',                                    &
                 'longitude_2d           ',                                    &
                 'rivers_outflow_number  ',                                    &
                 'rivers_storage         ',                                    &
                 'logn_mean              ',                                    &
                 'logn_stdev             ',                                    &
                 'res_capacity_grid      ',                                    &
                 'res_catch_grid         ',                                    &
                 'res_year_grid          ',                                    &
                 'res_critical_grid      ',                                    &
                 'res_flood_grid         ',                                    &
                 'res_emergency_grid     ',                                    &
                 'res_normal_release_grid',                                    &
                 'res_flood_release_grid ',                                    &
                 'minor_res_capacity_grid',                                    &
                 'minor_res_frac_grid    ']

DO i = 1, nvars
  IF ( .NOT. ANY( var_values(:) == var(i) ) ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "Unrecognised identifier '" // TRIM(var(i)) // "'")
  END IF
  IF ( var(i) == 'rivers_outflow_number' ) THEN
    IF ( .NOT. l_outflow_per_river ) THEN
      ! Diagnostic output is expected to be requested later in jules_output.
      WRITE(jules_message,*)                                                   &
         "'rivers_outflow_number' appears in var list; outflow_per_river " //  &
         "diagnostic is expected to be requested by jules_output."
      CALL log_info(RoutineName, jules_message)
      l_outflow_per_river = .TRUE.
    END IF
  END IF
  IF ( var(i) == 'rivers_storage' ) THEN
    WRITE(jules_message,*)                                                     &
       "'rivers_storage' appears in var list; this will be used to "       //  &
       "initialise 'rivers_sto_rp'."
    CALL log_info(RoutineName, jules_message)
    l_init_storage = .TRUE.
  END IF
END DO

!------------------------------------------------------------------------------
! Check that a file name was provided.
!------------------------------------------------------------------------------
IF ( LEN_TRIM(FILE) == 0 ) THEN
  CALL log_fatal( RoutineName, "No name given for file." )
END IF

! Set the name of the file to be used to read coordinates. This depends on
! whether variable-name templating or a list of files is used.
IF ( tpl_has_var_name(FILE) .OR. read_list ) THEN
  ! If templating is specified, or reading a list of files, check we have a
  ! coordinate_file.
  IF ( LEN_TRIM(coordinate_file) == 0 ) THEN
    CALL log_fatal( RoutineName, "No name given for coordinate_file." )
  ELSE
    file_name_coords = coordinate_file
  END IF
ELSE
  ! Use the only ancillary file.
  file_name_coords = FILE
  ! If another file was indicated, clarify that it will not be used.
  IF ( LEN_TRIM(coordinate_file) > 0 ) THEN
    CALL log_info( RoutineName,                                                &
       "No templating; coordinate_file will be ignored.")
  END IF
END IF

!------------------------------------------------------------------------------
! Regridding is only supported for (regular) lat-lon grids.
! The regularity is checked later.
!------------------------------------------------------------------------------
IF ( rivers_regrid .AND. .NOT. l_coord_latlon ) THEN
  CALL log_fatal( RoutineName,                                                 &
                 "Regridding can only be used with lat-lon grids." )
END IF

!------------------------------------------------------------------------------
! Insist on a river grid that is at least 2 x 2. This is required for the
! calculations of rivers_dx and rivers_dy.
!------------------------------------------------------------------------------
IF ( nx_rivers < 2  .OR. ny_rivers < 2 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "Routing grid must be at least 2x2. Dimensions provided: " //&
                  "nx_rivers = " // TRIM(to_string(nx_rivers))               //&
                  "ny_rivers = " // TRIM(to_string(ny_rivers)) )
END IF

!------------------------------------------------------------------------------
! land_dy and land_dx are required, and should be >0.
!------------------------------------------------------------------------------
IF ( ABS( land_dy - rmdi ) < EPSILON(1.0) .OR.                                 &
     ABS( land_dx - rmdi ) < EPSILON(1.0) ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "land_dy and land_dx must be provided." )
ELSE IF ( land_dx <= 0.0 .OR. land_dy <= 0.0 ) THEN
  CALL log_fatal( RoutineName,                                                 &
                  "land_dy and land_dx must be >0." )
END IF

!------------------------------------------------------------------------------
! Conditions for nx_land_grid, ny_land_grid, x1_land_grid and y1_land_grid.
!------------------------------------------------------------------------------
IF ( .NOT. l_find_grid ) THEN
  ! Values shoud be supplied. Note that we later test that nx_land_grid and
  ! ny_land_grid are large enough.
  IF ( nx_land_grid == imdi .OR. ny_land_grid == imdi .OR.                     &
       ABS( y1_land_grid - rmdi ) < EPSILON(1.0)  .OR.                         &
       ABS( x1_land_grid - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "nx_land_grid, ny_land_grid, x1_land_grid and "         // &
                    "y1_land_grid must be provided for this configuration." )
  ELSE IF ( nx_land_grid <= 0 .OR. ny_land_grid <= 0 ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "nx_land_grid and ny_land_grid must be >0.")
  END IF
ELSE
  ! l_find_grid = .TRUE.
  ! Values are not required as they will be calculated.
  IF ( nx_land_grid /= imdi .OR. ny_land_grid /= imdi .OR.                     &
       ABS( y1_land_grid - rmdi ) > EPSILON(1.0)  .OR.                         &
       ABS( x1_land_grid - rmdi ) > EPSILON(1.0) ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "nx_land_grid, ny_land_grid, x1_land_grid and "         // &
                    "y1_land_grid will be calculated and must not be "      // &
                    "provided for this configuration." )
  END IF
END IF  !  l_find_grid

!------------------------------------------------------------------------------
! Conditions for rivers_length.
!------------------------------------------------------------------------------
IF ( .NOT. l_coord_latlon ) THEN
  ! We don't have a latitude-longitude grid, and so must have rivers_length>0
  ! because that is used to calculate box areas.
  IF ( ABS( rivers_length - rmdi ) < EPSILON(1.0) .OR.                         &
       rivers_length <= 0.0 ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "rivers_length > 0 must be provided for this "          // &
                    "configuration." )
  END IF
END IF

! Note that this check requires that we have read the jules_overbank namelist.
IF ( i_river_vn == rivers_rfm .OR.                                             &
     ( l_riv_overbank .AND. ( overbank_model == overbank_simple .OR.           &
                              overbank_model == overbank_simple_rosgen ) )     &
   ) THEN
  ! These configurations require rivers_length. A value <=0 will later trigger
  ! calculation of rivers_length.
  IF ( ABS( rivers_length - rmdi ) < EPSILON(1.0) ) THEN
    CALL log_fatal( RoutineName,                                               &
                    "rivers_length must be provided for this configuration." )
  ELSE IF ( rivers_length <= 0.0 ) THEN
    ! A non-missing value has been provided but it is <= 0, hence a new value
    ! will be calculated - this is so as to mimic the behaviour of earlier
    ! versions of the code. Note that we will only get here for grids that are
    ! using lat and lon coords, because non-latlon grids were tested earlier
    ! to ensure rivers_length > 0.
    ! Set a switch to trigger later calculation of rivers_length.
    l_calc_rivers_length = .TRUE.
    CALL log_info( RoutineName,                                                &
                   "rivers_length <= 0: value will be calculated from grid." )
  END IF
END IF

! Outflow per river requires the use of the direction_grid variable which is
! only allocated if l_use_direction=T. At present this means it cannot be used
! with CaMa-Flood.
IF ( l_outflow_per_river .AND. i_river_vn == rivers_camaflood ) THEN
  CALL log_fatal(RoutineName, "River coupling is not yet possible with " //    &
                              "CaMa-Flood.")
END IF

RETURN
END SUBROUTINE check_jules_rivers_props

!##############################################################################

SUBROUTINE allocate_river_vars_grid( global_land_pts, nx_rivers, ny_rivers,    &
                                     l_use_direction,                          &
                                     rivers, rivers_data )

!------------------------------------------------------------------------------
! Description:
!   Allocate gridded river ancillary variables, initialise, and associate
!   pointers.
!------------------------------------------------------------------------------

USE jules_rivers_mod, ONLY: i_river_vn, rivers_trip, rivers_rfm,               &
                            rivers_camaflood,                                  &
                            l_reservoirs, l_minor_reservoirs, l_sea_level,     &
                            l_outflow_per_river, l_init_storage,               &
                            rivers_data_type, rivers_type

IMPLICIT NONE

!------------------------------------------------------------------------------
! Scalar arguments with INTENT(IN)
!------------------------------------------------------------------------------
INTEGER, INTENT(IN) ::                                                         &
  global_land_pts,                                                             &
    ! Number of land points.
  nx_rivers, ny_rivers
    ! Grid sizes.

LOGICAL, INTENT(IN) ::                                                         &
  l_use_direction
    ! Flag indicating type of flow direction ancillary inputs.
    ! TRUE  = a flow direction field is provided
    ! FALSE = fields of nextx and nexty (indices of the location of the next
    !         downstream point) are provided.

!------------------------------------------------------------------------------
! Arguments with INTENT(IN OUT)
!------------------------------------------------------------------------------
TYPE(rivers_type), INTENT(IN OUT) :: rivers
TYPE(rivers_data_type), INTENT(IN OUT), TARGET :: rivers_data

CHARACTER(LEN=*), PARAMETER :: RoutineName = 'ALLOCATE_RIVER_VARS_GRID'

INTEGER ::                                                                     &
  ERROR, error_sum,                                                            &
    ! Error values.
  nx_size, ny_size
    ! Sizes used for allocations.

!end of header
!------------------------------------------------------------------------------

! This routine is only called by the master task. All variables are allocated,
! but they are only allocated at full size when required by the run
! configuration.

! Variables that are always required.
ALLOCATE( grid_riv_pt_number(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = ERROR
ALLOCATE( nextx_grid(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( nexty_grid(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( river_mask(nx_rivers,ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( rivers_lat2d(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( rivers_lon2d(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR

! Flow direction - only needed if l_use_direction=T.
IF ( l_use_direction ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( rivers_dir(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( direction_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! nextx_grid and nexty_grid (REAL variables, for i.o) - only needed if
! l_use_direction=F.
IF ( .NOT. l_use_direction ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( river_nextx_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( river_nexty_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! CaMa-Flood variables.
IF ( i_river_vn == rivers_camaflood ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
! CaMa-Flood 2D ancillary variables.
ALLOCATE( channel_depth_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( channel_width_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( river_distance_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( river_elevation_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( river_length_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( river_manning_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( land_fraction_2d(nx_rivers, ny_rivers), STAT = ERROR )
error_sum = error_sum + ERROR

! CaMa-Flood variables that depend on a further switch.
IF ( i_river_vn == rivers_camaflood .AND. l_sea_level ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( mean_sea_level_grid(nx_size, ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! RFM variables.
IF ( i_river_vn == rivers_rfm ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( rivers_dra(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! TRIP variables.
IF ( i_river_vn == rivers_trip ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( rivers_seq(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! River coupling variables.
IF ( l_outflow_per_river ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( rivers_outflow_number(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! River storage.
IF ( l_init_storage ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( rivers_storage(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

! Reservoir 2D ancillary variables.
IF ( l_reservoirs ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( res_capacity_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( res_catch_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( res_year_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( res_critical_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( res_flood_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( res_emergency_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( res_normal_release_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( res_flood_release_grid(nx_size,ny_size), STAT = ERROR )
! Minor reservoir 2D ancillary variables.
IF ( l_minor_reservoirs ) THEN
  nx_size = nx_rivers
  ny_size = ny_rivers
ELSE
  nx_size = 1
  ny_size = 1
END IF
ALLOCATE( minor_res_capacity_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR
ALLOCATE( minor_res_frac_grid(nx_size,ny_size), STAT = ERROR )
error_sum = error_sum + ERROR

IF ( error_sum /= 0 ) THEN
  CALL log_fatal( RoutineName, "Error allocating for rivers arrays." )
END IF

! Initialise to missing data.
grid_riv_pt_number(:,:)          = imdi
nextx_grid(:,:)                  = imdi
nexty_grid(:,:)                  = imdi
river_mask(:,:)                  = imdi
rivers_lat2d(:,:)                = rmdi
rivers_lon2d(:,:)                = rmdi
rivers_dir(:,:)                  = rmdi
direction_grid(:,:)              = imdi
river_nextx_grid(:,:)            = rmdi
river_nexty_grid(:,:)            = rmdi
channel_depth_grid(:,:)          = rmdi
channel_width_grid(:,:)          = rmdi
river_distance_grid(:,:)         = rmdi
river_elevation_grid(:,:)        = rmdi
river_length_grid(:,:)           = rmdi
river_manning_grid(:,:)          = rmdi
rivers_dra(:,:)                  = rmdi
rivers_seq(:,:)                  = rmdi
rivers_outflow_number(:,:)       = rmdi
rivers_storage(:,:)              = rmdi
land_fraction_2d(:,:)            = rmdi
minor_res_capacity_grid(:,:)     = rmdi
minor_res_frac_grid(:,:)         = rmdi
res_capacity_grid(:,:)        = rmdi
res_catch_grid(:,:)           = rmdi
res_year_grid(:,:)            = rmdi
res_critical_grid(:,:)        = rmdi
res_flood_grid(:,:)           = rmdi
res_emergency_grid(:,:)       = rmdi
res_normal_release_grid(:,:)  = rmdi
res_flood_release_grid(:,:)   = rmdi

RETURN
END SUBROUTINE allocate_river_vars_grid

!##############################################################################

SUBROUTINE deallocate_river_props( )

!------------------------------------------------------------------------------
! Description:
!   Deallocate river variables that are not needed any further.
!------------------------------------------------------------------------------

IMPLICIT NONE

!------------------------------------------------------------------------------
! Variables that are only allocated in some configurations and/or on master
!  task - hence first check if allocated.
!------------------------------------------------------------------------------
IF ( ALLOCATED(direction_grid) ) THEN
  DEALLOCATE( direction_grid )
END IF

IF ( ALLOCATED(grid_riv_pt_number) ) THEN
  DEALLOCATE( grid_riv_pt_number )
END IF

IF ( ALLOCATED(nextx_grid) ) THEN
  DEALLOCATE( nextx_grid )
END IF

IF ( ALLOCATED(nexty_grid) ) THEN
  DEALLOCATE( nexty_grid )
END IF

IF ( ALLOCATED(river_mask) ) THEN
  DEALLOCATE( river_mask )
END IF

IF ( ALLOCATED(river_nextx_grid) ) THEN
  DEALLOCATE( river_nextx_grid )
END IF

IF ( ALLOCATED(river_nexty_grid) ) THEN
  DEALLOCATE( river_nexty_grid )
END IF

IF ( ALLOCATED(rivers_lat2d) ) THEN
  DEALLOCATE( rivers_lat2d )
END IF

IF ( ALLOCATED(rivers_lon2d) ) THEN
  DEALLOCATE( rivers_lon2d )
END IF

IF ( ALLOCATED(rivers_dir) ) THEN
  DEALLOCATE( rivers_dir )
END IF

IF ( ALLOCATED(land_fraction_2d) ) THEN
  DEALLOCATE( land_fraction_2d )
END IF

IF ( ALLOCATED(rivers_dra) ) THEN
  DEALLOCATE( rivers_dra )
END IF

IF ( ALLOCATED(rivers_seq) ) THEN
  DEALLOCATE( rivers_seq )
END IF

IF ( ALLOCATED(rivers_outflow_number) ) THEN
  DEALLOCATE( rivers_outflow_number )
END IF

IF ( ALLOCATED(rivers_storage) ) THEN
  DEALLOCATE( rivers_storage )
END IF

! CaMa-Flood variables.
IF ( ALLOCATED( channel_depth_grid ) ) THEN
  DEALLOCATE( channel_depth_grid )
END IF

IF ( ALLOCATED( channel_width_grid ) ) THEN
  DEALLOCATE( channel_width_grid )
END IF

IF ( ALLOCATED( river_distance_grid ) ) THEN
  DEALLOCATE( river_distance_grid )
END IF

IF ( ALLOCATED( river_elevation_grid ) ) THEN
  DEALLOCATE( river_elevation_grid )
END IF

IF ( ALLOCATED( river_length_grid ) ) THEN
  DEALLOCATE( river_length_grid )
END IF

IF ( ALLOCATED( river_manning_grid ) ) THEN
  DEALLOCATE( river_manning_grid )
END IF

! Reservoir variables.
IF ( ALLOCATED( res_capacity_grid ) ) THEN
  DEALLOCATE( res_capacity_grid )
END IF
IF ( ALLOCATED( res_catch_grid ) ) THEN
  DEALLOCATE( res_catch_grid )
END IF
IF ( ALLOCATED( res_year_grid ) ) THEN
  DEALLOCATE( res_year_grid )
END IF
IF ( ALLOCATED( res_critical_grid ) ) THEN
  DEALLOCATE( res_critical_grid )
END IF
IF ( ALLOCATED( res_flood_grid ) ) THEN
  DEALLOCATE( res_flood_grid )
END IF
IF ( ALLOCATED( res_emergency_grid ) ) THEN
  DEALLOCATE( res_emergency_grid )
END IF
IF ( ALLOCATED( res_normal_release_grid ) ) THEN
  DEALLOCATE( res_normal_release_grid )
END IF
IF ( ALLOCATED( res_flood_release_grid ) ) THEN
  DEALLOCATE( res_flood_release_grid )
END IF

! Minor reservoir variables.
IF ( ALLOCATED( minor_res_capacity_grid ) ) THEN
  DEALLOCATE( minor_res_capacity_grid )
END IF

IF ( ALLOCATED( minor_res_frac_grid ) ) THEN
  DEALLOCATE( minor_res_frac_grid )
END IF

RETURN
END SUBROUTINE deallocate_river_props

!##############################################################################

END MODULE jules_rivers_props_mod
#endif

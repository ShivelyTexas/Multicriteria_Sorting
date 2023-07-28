
!  ***************************************************************************************************************
!
!    Shared Data 
!   
!  ***************************************************************************************************************
!
!
module Shared_Constants
!
!  ***************************************************************************************************************
!     
!   Module used to share constants among program, subroutine and functions
!
!  ***************************************************************************************************************

      implicit none
      save
      integer, parameter :: single = selected_real_kind(6,30)
      integer, parameter :: double = selected_real_kind(14,300)
      real (kind = double), parameter :: twopi = 6.283185307d0

end module Shared_Constants
!
!
module Shared_Data
!
!  ***************************************************************************************************************
!     
!   Module used to share data among program, subroutines and functions
!
!  ***************************************************************************************************************

      use Shared_Constants
      implicit none
      save
      character (len = 40) :: does_bound_exist, type_of_bound_to_compute
      integer :: nobs, nobs_used_for_estimation, nobs_not_used_for_estimation, nreg, n_alternatives_pass, k_for_kSLA_rule, n_discrete_prior_beta,    &
        max_k_for_kSLA_rule, max_k_for_kSLA_rule_lower, max_k_for_kSLA_rule_upper, i_obs_other
      integer, allocatable, dimension (:) :: istudent_lower, istudent_upper, n_discrete_prior_beta_nreg_ge_1
      real (kind = double) :: cost_misplace, cost_admit_should_reject, cost_reject_should_admit, cost_ask_dm, beta_pass,                             &
        dlower_bound_using_exp_value_of_myopic_exp_losses_given_beta, prior_beta_lower_bound_gaussian, prior_beta_upper_bound_gaussian,              &
        prob_threshold, prob_threshold_for_xeq1_for_prior_beta_lower_bound, prob_threshold_for_xeq0_for_prior_beta_upper_bound, z_cutoff_pass,       &
        percentage_pass, specified_x_value_pass, percentage_accept_global_for_prior_on_beta,                                                                           &
        g_multiplier_for_p0, g_multiplier_for_p1, g_multiplier_for_p10, g_multiplier_for_p01, g_multiplier_for_mean_x_eq_1
      real (kind = double), allocatable, dimension (:) :: beta_vec_pass, discrete_prior_beta_prob, prob_Ii_eq_1, z_cutoff_for_each_beta,             &
        discrete_prior_beta_prob_store, prob_Ij_eq_1
      real (kind = double), allocatable, dimension (:,:) :: X_matrix, X_matrix_not_used_for_estimation,                                              &
        prob_Ij_eq_1_given_beta_m, prob_Ij_eq_2_given_beta_m, discrete_prior_beta_value_nreg_ge_1

end module Shared_Data


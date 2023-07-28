!
!
subroutine Compute_lower_and_upper_bound_for_prior_on_beta_1 (dummy_argument)

!  *********************************************************************************************************************
!
!   Subroutine to compute lower and upper bounds for prior on beta
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      use zbren_int
      implicit none
!
!   Subroutine argument
!
      real (kind = double) :: dummy_argument
!
!   Local arguments
!
      integer :: maxfn
      real (kind = double) :: dlower_bound, upper_bound, fcn_value_at_lower_bound, fcn_value_at_upper_bound, errabs, errrel
      real (kind = double), external :: f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a,                                       &
        f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b
!
!   Line to avoid compilation error
!
      dummy_argument = 1.d0
      does_bound_exist = 'Bound_exists'
!
!   Compute lower bound for prior on beta. First, compute lower and upper bounds for function zero and then zbren to find function zero
!
      specified_x_value_pass = X_matrix(nobs,1)
      prob_threshold = prob_threshold_for_xeq1_for_prior_beta_lower_bound
      dlower_bound = 0.d0
      fcn_value_at_lower_bound = f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (dlower_bound)
      if (fcn_value_at_lower_bound > 0.d0) then
        prior_beta_lower_bound_gaussian = 0.d0
        goto 250
      endif
100   upper_bound = dlower_bound + 1.d0
      if (upper_bound > 100.d0) then
        does_bound_exist = 'No_bound_exists'
        return
      endif
      fcn_value_at_upper_bound = f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (upper_bound)
      if (fcn_value_at_upper_bound < -1.d-6) then
        dlower_bound = upper_bound
        goto 100
      elseif (fcn_value_at_upper_bound > 1.d-6) then
        goto 200
      else
        prior_beta_lower_bound_gaussian = upper_bound
        goto 250
      endif
200   errabs = 1.d-8; errrel = 1.d-8; maxfn = 200
      call zbren (f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b, dlower_bound, upper_bound, errabs, errrel, maxfn)
      prior_beta_lower_bound_gaussian = upper_bound
!
!   Compute upper bound for prior on beta. First, compute lower and upper bounds for function zero and then zbren to find function zero
!
250   prob_threshold = prob_threshold_for_xeq0_for_prior_beta_upper_bound
      specified_x_value_pass = X_matrix(1,1)
      dlower_bound = 0.d0
      fcn_value_at_lower_bound = f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (dlower_bound)
300   upper_bound = dlower_bound + 1.d0
      if (upper_bound > 100.d0) then
        does_bound_exist = 'No_bound_exists'
        return
      endif
      fcn_value_at_upper_bound = f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (upper_bound)
      if (fcn_value_at_upper_bound > 1.d-6) then
        dlower_bound = upper_bound
        goto 300
      elseif (fcn_value_at_upper_bound < -1.d-6) then
        goto 400
      else
        prior_beta_lower_bound_gaussian = upper_bound
        goto 9990
      endif
400   errabs = 1.d-8; errrel = 1.d-8; maxfn = 200
      call zbren (f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b, dlower_bound, upper_bound, errabs, errrel, maxfn)
      prior_beta_upper_bound_gaussian = upper_bound

9990  return
end subroutine Compute_lower_and_upper_bound_for_prior_on_beta_1
!
!
subroutine Compute_lower_and_upper_bound_for_prior_on_beta_nreg_eq_2_1 (dummy_argument)

!  *********************************************************************************************************************
!
!   Subroutine to compute lower and upper bounds for prior on beta when nreg=2
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      use zbren_int
      implicit none
!
!   Subroutine argument
!
      real (kind = double) :: dummy_argument
!
!   Local arguments
!
      integer :: maxfn
      real (kind = double) :: dlower_bound, upper_bound, fcn_value_at_lower_bound, fcn_value_at_upper_bound, errabs, errrel
      real (kind = double), external :: f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a,                                       &
        f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b
!
!   Line to avoid compilation error
!
      dummy_argument = 1.d0
      does_bound_exist = 'Bound_exists'
!
!   Compute lower bound for prior on beta. First, compute lower and upper bounds for function zero and then zbren to find function zero
!
      specified_x_value_pass = X_matrix(nobs,1)
      prob_threshold = prob_threshold_for_xeq1_for_prior_beta_lower_bound
      dlower_bound = 0.d0
      fcn_value_at_lower_bound = f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (dlower_bound)
      if (fcn_value_at_lower_bound > 0.d0) then
        prior_beta_lower_bound_gaussian = 0.d0
        goto 250
      endif
100   upper_bound = dlower_bound + 1.d0
      if (upper_bound > 100.d0) then
        does_bound_exist = 'No_bound_exists'
        return
      endif
      fcn_value_at_upper_bound = f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (upper_bound)
      if (fcn_value_at_upper_bound < -1.d-6) then
        dlower_bound = upper_bound
        goto 100
      elseif (fcn_value_at_upper_bound > 1.d-6) then
        goto 200
      else
        prior_beta_lower_bound_gaussian = upper_bound
        goto 250
      endif
200   errabs = 1.d-8; errrel = 1.d-8; maxfn = 200
      call zbren (f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b, dlower_bound, upper_bound, errabs, errrel, maxfn)
      prior_beta_lower_bound_gaussian = upper_bound
!
!   Compute upper bound for prior on beta. First, compute lower and upper bounds for function zero and then zbren to find function zero
!
250   prob_threshold = prob_threshold_for_xeq0_for_prior_beta_upper_bound
      specified_x_value_pass = X_matrix(1,1)
      dlower_bound = 0.d0
      fcn_value_at_lower_bound = f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (dlower_bound)
300   upper_bound = dlower_bound + 1.d0
      if (upper_bound > 100.d0) then
        does_bound_exist = 'No_bound_exists'
        return
      endif
      fcn_value_at_upper_bound = f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (upper_bound)
      if (fcn_value_at_upper_bound > 1.d-6) then
        dlower_bound = upper_bound
        goto 300
      elseif (fcn_value_at_upper_bound < -1.d-6) then
        goto 400
      else
        prior_beta_lower_bound_gaussian = upper_bound
        goto 9990
      endif
400   errabs = 1.d-8; errrel = 1.d-8; maxfn = 200
      call zbren (f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b, dlower_bound, upper_bound, errabs, errrel, maxfn)
      prior_beta_upper_bound_gaussian = upper_bound

9990  return
end subroutine Compute_lower_and_upper_bound_for_prior_on_beta_nreg_eq_2_1
!
!
real (kind = double) function f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a (beta)
!
!*******************************************************************************************************
!
!   Double precision function
!
!*******************************************************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Function argument
!
      real (kind = double) :: beta
!
!   Local argument
!
      real (kind = double) :: temp1
      real (kind = double) :: Determine_z_cutoff_given_beta_discrete_prior_1
!
!   Compute pr[I=2 | x, beta)
!
      temp1 = Determine_z_cutoff_given_beta_discrete_prior_1 (beta)
      f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a = (1.d0 - anordf(z_cutoff_pass - specified_x_value_pass * beta)) - prob_threshold

      return
end function f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1a
!
!
real (kind = double) function f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b (beta)
!
!*******************************************************************************************************
!
!   Double precision function
!
!*******************************************************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Function argument
!
      real (kind = double) :: beta
!
!   Local argument
!
      real (kind = double) :: temp1
      real (kind = double) :: Determine_z_cutoff_given_beta_discrete_prior_1
!
!   Compute pr[I=2 | x, beta)
!
      temp1 = Determine_z_cutoff_given_beta_discrete_prior_1 (beta)
      f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b = (1.d0 - anordf(z_cutoff_pass - specified_x_value_pass * beta)) - prob_threshold

      return
end function f_compute_prob_Ieq2_given_x_and_beta_minus_prob_threshold_1b
!
!
subroutine Compute_prob_Ij_eq_1_1 (dummy_argument)

!  *********************************************************************************************************************
!
!   Subroutine to compute P(I_j=1)
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      implicit none
!
!   Subroutine argument
!
      real (kind = double) :: dummy_argument
!
!   Local arguments
!
      integer :: j
      real (kind = double) :: wk1(1:n_discrete_prior_beta)
!
!   Line to avoid compilation error
!
      dummy_argument = 1.d0
!
!   Compute pr(I(j)=1)
!
      do j = 1, nobs
        wk1(1:n_discrete_prior_beta) = prob_Ij_eq_1_given_beta_m(j,1:n_discrete_prior_beta) * discrete_prior_beta_prob(1:n_discrete_prior_beta)
        prob_Ij_eq_1(j) = sum(wk1(1:n_discrete_prior_beta))
      enddo

9990  return
end subroutine Compute_prob_Ij_eq_1_1
!
!
subroutine Compute_lower_bd_for_full_tree_given_prior_for_beta_nreg_ge_1_1 (dummy_argument)

!  *********************************************************************************************************************
!
!   Subroutine to compute lower bound for full tree given probability distribution for beta          
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Subroutine argument
!
      real (kind = double) :: dummy_argument
!
!   Local arguments
!
      integer :: i, j, m
      integer :: istudent(nobs,n_discrete_prior_beta)
      real (kind = double) :: temp1, expected_cost_admit_should_reject_times_prob_reject_sub, expected_cost_reject_should_admit_times_prob_admit_sub
      real (kind = double) :: expected_cost_myopic_rule_given_beta(n_discrete_prior_beta), wk1_nobs(nobs), wk2_nobs(nobs)
!
!   Line to avoid compilation error
!
      dummy_argument = 1.d0
!
!   Compute probabilities and expected costs given beta_m
!
      do m = 1, n_discrete_prior_beta
        expected_cost_myopic_rule_given_beta(m) = 0.d0
        do j = 1, nobs
          istudent(j,m) = 0
          temp1 = cost_ask_DM
          expected_cost_admit_should_reject_times_prob_reject_sub = cost_admit_should_reject * (1.d0 - prob_Ij_eq_2_given_beta_m(j,m))
          if (expected_cost_admit_should_reject_times_prob_reject_sub < temp1) then
            temp1 = expected_cost_admit_should_reject_times_prob_reject_sub
            istudent(j,m) = 2
          endif
          expected_cost_reject_should_admit_times_prob_admit_sub = cost_reject_should_admit * prob_Ij_eq_2_given_beta_m(j,m)
          if (expected_cost_reject_should_admit_times_prob_admit_sub < temp1) then
            temp1 = expected_cost_reject_should_admit_times_prob_admit_sub
            istudent(j,m) = 1
          endif
          expected_cost_myopic_rule_given_beta(m) = expected_cost_myopic_rule_given_beta(m) + temp1
        enddo
      enddo
!
!   Determine SA, S1 and S2 alternatives
!
      do i = 1, nobs
        istudent_lower(i) = 9
        istudent_upper(i) = 9
        wk1_nobs(i) = minval(cost_reject_should_admit * prob_Ij_eq_2_given_beta_m(i,1:n_discrete_prior_beta))
        wk2_nobs(i) = minval(cost_admit_should_reject * (1.d0 - prob_Ij_eq_2_given_beta_m(i,1:n_discrete_prior_beta)))
      enddo
      do i = 1, nobs
!
!   Check for SA alternatives
!
        if (wk1_nobs(i) > cost_ask_DM .and. wk2_nobs(i) > cost_ask_DM) then
          istudent_lower(i) = 0
          istudent_upper(i) = 0
        endif
      enddo
!
!   Compute expected value with respect to beta
!
      dlower_bound_using_exp_value_of_myopic_exp_losses_given_beta                                                                         &
        = sum(discrete_prior_beta_prob(1:n_discrete_prior_beta) * expected_cost_myopic_rule_given_beta(1:n_discrete_prior_beta))

9990  return
end subroutine Compute_lower_bd_for_full_tree_given_prior_for_beta_nreg_ge_1_1
!
!
subroutine Compute_minimum_expected_cost_decision_node_stage1_all_k_1 (n_alternatives, n_alternatives_stage1, j_vector, k_vector,           &
   indices_stage1, djoint_prob_previous_Is, dmin_exp_cost_dec_node_stage1)

!  *********************************************************************************************************************
!
!   Subroutine to compute minimum expected cost at stage 1
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      implicit none
!
!   Subroutine arguments
!
      integer :: n_alternatives, n_alternatives_stage1
      integer :: j_vector(n_alternatives), k_vector(n_alternatives), indices_stage1(n_alternatives)
      real (kind = double) :: djoint_prob_previous_Is, dmin_exp_cost_dec_node_stage1
!
!   Local arguments
!
      integer :: i1, j1
      real (kind = double) :: prob_Ij_eq_1_given_previous_Is, temp1,                                                                   &
        cost_admit_should_reject_times_prob_reject, cost_reject_should_admit_times_prob_admit, dlower_bound
      real (kind = double) :: post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta)
!
!   Compute pr[beta=beta1|I(j2)=k2,I(j3)=k3], pr[beta=beta2|I(j2)=k2,I(j3)=k3] and pr[I(j2)=k2,I(j3)=k3]
!
      dmin_exp_cost_dec_node_stage1 = dble(k_for_kSLA_rule) * cost_ask_DM
      call Compute_post_prob_beta_given_previous_Is_stage1_all_k_1 (j_vector, k_vector, djoint_prob_previous_Is, post_prob_beta_m_given_previous_Is)
      if (type_of_bound_to_compute == 'Upper_bound') then
        do i1 = 1, n_alternatives_stage1
          j1 = indices_stage1(i1)
!
!   Compute prob_Ij_eq_1_given_previous_Is
!
          prob_Ij_eq_1_given_previous_Is =                                                                                                &
            sum(prob_Ij_eq_1_given_beta_m(j1,1:n_discrete_prior_beta) * post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta))
          cost_admit_should_reject_times_prob_reject = cost_admit_should_reject * prob_Ij_eq_1_given_previous_Is
          cost_reject_should_admit_times_prob_admit = cost_reject_should_admit * (1.d0 - prob_Ij_eq_1_given_previous_Is)
          temp1 = cost_ask_DM
          if (cost_admit_should_reject_times_prob_reject < temp1) temp1 = cost_admit_should_reject_times_prob_reject
          if (cost_reject_should_admit_times_prob_admit < temp1) temp1 = cost_reject_should_admit_times_prob_admit
          dmin_exp_cost_dec_node_stage1 = dmin_exp_cost_dec_node_stage1 + temp1
        enddo
      elseif (type_of_bound_to_compute == 'Lower_bound') then
        call Compute_low_bd_given_prob_Ieq1_prob_dist_for_beta_stage1_keq1_1 (n_alternatives_stage1, indices_stage1,                        &
          post_prob_beta_m_given_previous_Is, dlower_bound)
          dmin_exp_cost_dec_node_stage1 = dmin_exp_cost_dec_node_stage1 + dlower_bound
      endif

9990  return
end subroutine Compute_minimum_expected_cost_decision_node_stage1_all_k_1
!
!
subroutine Compute_post_prob_beta_given_previous_Is_stage1_all_k_1 (j_vector, k_vector, prob_previous_Is, post_prob_beta_m_given_previous_Is)

!  *********************************************************************************************************************
!
!   Subroutine to compute posterior probabilities for beta given previous I values
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      implicit none
!
!   Subroutine arguments
!
      integer :: i
      integer :: j_vector(nobs), k_vector(nobs)
      real (kind = double) :: prob_previous_Is
      real (kind = double) :: post_prob_beta_m_given_previous_Is(n_discrete_prior_beta)
!
!   Local arguments
!
      real (kind = double) :: prob_Ij_given_beta_m(2,nobs,n_discrete_prior_beta)      !, wk1(n_discrete_prior_beta)
!
!   Set pr(I|beta) vectors to use below
!
      prob_Ij_given_beta_m(1,1:nobs,1:n_discrete_prior_beta) = prob_Ij_eq_1_given_beta_m(1:nobs,1:n_discrete_prior_beta)
      prob_Ij_given_beta_m(2,1:nobs,1:n_discrete_prior_beta) = 1.d0 - prob_Ij_given_beta_m(1,1:nobs,1:n_discrete_prior_beta)
!
!   Compute posterior pr(beta|previous Is)
!
      post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta)
      do i = 1, k_for_kSLA_rule
        post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta) = post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta)                  &
          * prob_Ij_given_beta_m(k_vector(i),j_vector(i),1:n_discrete_prior_beta)
      enddo
      prob_previous_Is = sum(post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta))
      post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta) = post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta) / prob_previous_Is

9990  return
end subroutine Compute_post_prob_beta_given_previous_Is_stage1_all_k_1
!
!
subroutine Compute_low_bd_given_prob_Ieq1_prob_dist_for_beta_stage1_keq1_1 (n_alternatives_stage1, indices_stage1,                   &
                                                                            post_prob_beta_m_given_previous_Is, dlower_bound_keq1)

!  ************************************************************************************************************************
!
!   Subroutine to compute lower bound for a tree given n alternatives, prob[I(i)=1] and probability distribution for beta
!
!  ************************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      implicit none
!
!   Subroutine argument
!
      integer :: n_alternatives_stage1
      integer :: indices_stage1(nobs)
      real (kind = double) :: dlower_bound_keq1
      real (kind = double) :: post_prob_beta_m_given_previous_Is(n_discrete_prior_beta)
!
!   Local arguments
!
      integer :: i1, j1, m
      real (kind = double) :: temp1, expected_cost_admit_should_reject_times_prob_reject_sub, expected_cost_reject_should_admit_times_prob_admit_sub
      real (kind = double) :: expected_cost_myopic_rule_given_beta_m(n_discrete_prior_beta)
!
!   Compute probabilities and expected costs - Vectorization did not appear to speed up these calculations - See Sec50_d and Sec50_e 
!
      do m = 1, n_discrete_prior_beta
        expected_cost_myopic_rule_given_beta_m(m) = 0.d0
        do i1 = 1, n_alternatives_stage1
          j1 = indices_stage1(i1)
          temp1 = cost_ask_DM
          expected_cost_admit_should_reject_times_prob_reject_sub = cost_admit_should_reject * prob_Ij_eq_1_given_beta_m(j1,m)
          if (expected_cost_admit_should_reject_times_prob_reject_sub < temp1) temp1 = expected_cost_admit_should_reject_times_prob_reject_sub
          expected_cost_reject_should_admit_times_prob_admit_sub = cost_reject_should_admit * (1.d0 - prob_Ij_eq_1_given_beta_m(j1,m))
          if (expected_cost_reject_should_admit_times_prob_admit_sub < temp1) temp1 = expected_cost_reject_should_admit_times_prob_admit_sub
          expected_cost_myopic_rule_given_beta_m(m) = expected_cost_myopic_rule_given_beta_m(m) + temp1
        enddo
      enddo
!
!   Compute expected value with respect to beta
!
      dlower_bound_keq1                                                                                                                       &
        = sum(post_prob_beta_m_given_previous_Is(1:n_discrete_prior_beta) * expected_cost_myopic_rule_given_beta_m(1:n_discrete_prior_beta))

9990  return
end subroutine Compute_low_bd_given_prob_Ieq1_prob_dist_for_beta_stage1_keq1_1
!
!
subroutine Compute_z_cutoff_given_beta_m_nreg_ge_1_1 (dummy_argument)

!  *********************************************************************************************************************
!
!   Subroutine
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Subroutine argument
!
      real (kind = double) :: dummy_argument
!
!   Local arguments
!
      integer :: m
      real (kind = double) :: temp1
      real (kind = double) :: beta_vec(nreg)
      real (kind = double) :: Determine_z_cutoff_given_beta_discrete_prior_nreg_ge_1_1
!
!   Line to avoid compilation error
!
      dummy_argument = 1.d0
!
!   Compute pr[I(j)=1 | Beta) and pr[I(j)=2 | Beta) for each beta
!
      do m = 1, n_discrete_prior_beta
        beta_vec(1:nreg) = discrete_prior_beta_value_nreg_ge_1(m,1:nreg)
        temp1 = Determine_z_cutoff_given_beta_discrete_prior_nreg_ge_1_1 (beta_vec(1:nreg))
        z_cutoff_for_each_beta(m) = z_cutoff_pass
      enddo

9990  return
end subroutine Compute_z_cutoff_given_beta_m_nreg_ge_1_1
!
!
real (kind = double) function Determine_z_cutoff_given_beta_discrete_prior_1 (beta)
!
!**********************************************************************
!
!   Double precision function 
!
!**********************************************************************

      use Shared_Constants
      use Shared_Data
      use zbren_int
      implicit none
!
!   Subroutine arguments
!
      real (kind = double) :: beta
!
!   Local arguments
!
      real (kind = double) :: z_cutoff(2)
      integer :: nroot, itmax
      real (kind = double) :: errabs, errrel, eps, eta, z_lower, z_upper, f_lower, f_upper
      real (kind = double), external :: Equation_to_solve_for_zcutoff_given_beta_1a, Equation_to_solve_for_zcutoff_given_beta_1b
!
!   Line to avoid compilation error
!
      Determine_z_cutoff_given_beta_discrete_prior_1 = 1.d0
!
!   Set z_cutoff - Note that percentage_pass must be set in calling program
!
      beta_pass = beta
      z_lower = -0.5d0; z_upper = 0.5d0
      f_lower = Equation_to_solve_for_zcutoff_given_beta_1a(z_lower)
      f_upper = Equation_to_solve_for_zcutoff_given_beta_1a(z_upper)
40    if (f_lower >= 0.d0 .and. f_upper <= 0.d0) then
        goto 45
      elseif (f_upper > 0.d0) then
        z_lower = z_upper
        f_lower = f_upper
        z_upper = 2.d0 * z_upper
        f_upper = Equation_to_solve_for_zcutoff_given_beta_1a(z_upper)
        goto 40
      elseif (f_lower < 0.d0) then
        z_upper = z_lower
        f_upper = f_lower
        z_lower = 2.d0 * z_lower
        f_lower = Equation_to_solve_for_zcutoff_given_beta_1a(z_lower)
        goto 40
      else
        print *, 'Trouble in f_beta_z_cutoff_discrete_prior_1'
        stop
      endif
45    errabs = 1.d-8; errrel = 1.d-8; eps = 1.d-8; eta = 0.01; nroot = 1; itmax = 100
      call zbren (Equation_to_solve_for_zcutoff_given_beta_1b, z_lower, z_upper, errabs, errrel, itmax)
      z_cutoff(1) = z_upper
      z_cutoff_pass = z_cutoff(1)

      return
end function Determine_z_cutoff_given_beta_discrete_prior_1
!
!
real (kind = double) function Determine_z_cutoff_given_beta_discrete_prior_nreg_ge_1_1 (beta_vec)
!
!**********************************************************************
!
!   Double precision function 
!
!**********************************************************************

      use Shared_Constants
      use Shared_Data
      use zbren_int
      implicit none
!
!   Subroutine arguments
!
      real (kind = double) :: beta_vec(nreg)
!
!   Local arguments
!
      real (kind = double) :: z_cutoff(2)
      integer :: nroot, itmax
      real (kind = double) :: errabs, errrel, eps, eta, z_lower, z_upper, f_lower, f_upper
      real (kind = double), external :: Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a, Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1b
!
!   Line to avoid compilation error
!
      Determine_z_cutoff_given_beta_discrete_prior_nreg_ge_1_1 = 1.d0
!
!   Set z_cutoff - Note that percentage_pass must be set in calling program
!
      beta_vec_pass(1:nreg) = beta_vec(1:nreg)
      z_lower = -0.5d0; z_upper = 0.5d0
      f_lower = Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a(z_lower)
      f_upper = Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a(z_upper)
40    if (f_lower >= 0.d0 .and. f_upper <= 0.d0) then
        goto 45
      elseif (f_upper > 0.d0) then
        z_lower = z_upper
        f_lower = f_upper
        z_upper = 2.d0 * z_upper
        f_upper = Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a(z_upper)
        goto 40
      elseif (f_lower < 0.d0) then
        z_upper = z_lower
        f_upper = f_lower
        z_lower = 2.d0 * z_lower
        f_lower = Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a(z_lower)
        goto 40
      else
        print *, 'Trouble in f_beta_z_cutoff_discrete_prior_nreg_ge_1_1'
        stop
      endif
45    errabs = 1.d-8; errrel = 1.d-8; eps = 1.d-8; eta = 0.01; nroot = 1; itmax = 100
      call zbren (Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1b, z_lower, z_upper, errabs, errrel, itmax)
      z_cutoff(1) = z_upper
      z_cutoff_pass = z_cutoff(1)

      return
end function Determine_z_cutoff_given_beta_discrete_prior_nreg_ge_1_1
!
!
real (kind = double) function Equation_to_solve_for_zcutoff_given_beta_1a (z_cutoff)
!
!**********************************************************************
!
!   Double precision function 
!
!**********************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Subroutine arguments
!
      real (kind = double) :: z_cutoff
!
!   Local arguments
!
      integer :: i
      real (kind = double) :: beta, percentage
!
!   Set beta and percentage values
!
      beta = beta_pass
      percentage = percentage_pass
!
!   Compute function value
!
      Equation_to_solve_for_zcutoff_given_beta_1a = -dble(nobs) * percentage
      do i = 1, nobs
        Equation_to_solve_for_zcutoff_given_beta_1a = Equation_to_solve_for_zcutoff_given_beta_1a                     &
          + (1.d0 - anordf(z_cutoff - beta * X_matrix(i,1)))
      enddo

      return
end function Equation_to_solve_for_zcutoff_given_beta_1a
!
!
real (kind = double) function Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a (z_cutoff)
!
!**********************************************************************
!
!   Double precision function 
!
!**********************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Subroutine arguments
!
      real (kind = double) :: z_cutoff
!
!   Local arguments
!
      integer :: i
      real (kind = double) :: percentage
      real (kind = double) :: beta_vec(nreg)
!
!   Set beta and percentage values
!
      beta_vec(1:nreg) = beta_vec_pass(1:nreg)
      percentage = percentage_pass
!
!   Compute function value
!
      Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a = -dble(nobs) * percentage
      do i = 1, nobs
        Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a = Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a                     &
          + (1.d0 - anordf(z_cutoff - sum(beta_vec(1:nreg) * X_matrix(i,1:nreg))))
      enddo

      return
end function Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1a
!
!
real (kind = double) function Equation_to_solve_for_zcutoff_given_beta_1b (z_cutoff)
!
!**********************************************************************
!
!   Double precision function 
!
!**********************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Subroutine arguments
!
      real (kind = double) :: z_cutoff
!
!   Local arguments
!
      integer :: i
      real (kind = double) :: beta, percentage
!
!   Set beta and percentage values
!
      beta = beta_pass
      percentage = percentage_pass
!
!   Compute function value
!
      Equation_to_solve_for_zcutoff_given_beta_1b = -dble(nobs) * percentage
      do i = 1, nobs
        Equation_to_solve_for_zcutoff_given_beta_1b = Equation_to_solve_for_zcutoff_given_beta_1b                     &
          + (1.d0 - anordf(z_cutoff - beta * X_matrix(i,1)))
      enddo

      return
end function Equation_to_solve_for_zcutoff_given_beta_1b
!
!
real (kind = double) function Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1b (z_cutoff)
!
!**********************************************************************
!
!   Double precision function 
!
!**********************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Subroutine arguments
!
      real (kind = double) :: z_cutoff
!
!   Local arguments
!
      integer :: i
      real (kind = double) :: percentage
      real (kind = double) :: beta_vec(nreg)
!
!   Set beta and percentage values
!
      beta_vec(1:nreg) = beta_vec_pass(1:nreg)
      percentage = percentage_pass
!
!   Compute function value
!
      Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1b = -dble(nobs) * percentage
      do i = 1, nobs
        Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1b = Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1b                     &
          + (1.d0 - anordf(z_cutoff - sum(beta_vec(1:nreg) * X_matrix(i,1:nreg))))
      enddo

      return
end function Equation_to_solve_for_zcutoff_given_beta_nreg_ge_1_1b
!
!
subroutine Compute_prob_Ij_eq_1_given_beta_m_values_nreg_ge_1_1 (dummy_argument)

!  *********************************************************************************************************************
!
!   Subroutine
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      use anordf_int
      implicit none
!
!   Subroutine argument
!
      real (kind = double) :: dummy_argument
!
!   Local arguments
!
      integer :: j, m
      real (kind = double) :: beta_vec(nreg)
!
!   Line to avoid compilation error
!
      dummy_argument = 1.d0
!
!   Compute pr[I(j)=1 | Beta) and pr[I(j)=2 | Beta) for each beta
!
      do m = 1, n_discrete_prior_beta
        do j = 1, nobs
          beta_vec(1:nreg) = discrete_prior_beta_value_nreg_ge_1(m,1:nreg)
          prob_Ij_eq_1_given_beta_m(j,m) = anordf(z_cutoff_for_each_beta(m) - sum(X_matrix(j,1:nreg) * beta_vec(1:nreg)))
          prob_Ij_eq_2_given_beta_m(j,m) = 1.d0 - prob_Ij_eq_1_given_beta_m(j,m)
        enddo
      enddo

9990  return
end subroutine Compute_prob_Ij_eq_1_given_beta_m_values_nreg_ge_1_1
!
!
subroutine Compute_iMat_and_initial_joint_probs_for_discrete_prior_1 (iMat, prob_joint_I)

!  *********************************************************************************************************************
!
!   Compute initial joint probabilities
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      use qdagi_int
      implicit none
!
!   Subroutine arguments
!
      integer :: iMat(2**nobs,nobs)
      real (kind = double) :: prob_joint_I(2**nobs)
!
!   Local arguments
!
      integer :: i, j, jtemp, itemp1, itemp2, itemp3, itemp4, itemp5, i_discrete_prior_beta
      real (kind = double) :: temp1
!
!   Construct matrix used to specify probabilities
!
      do j = 1, nobs
        do i= 1, 2 ** (j - 1)
          jtemp = nobs - j + 1
          itemp1 = 2 ** jtemp
          itemp2 = (2 ** jtemp) - 1
          itemp3 = 2 ** (jtemp - 1)
          itemp4 = itemp3 - 1
          itemp5 = 0
          iMat(itemp1 * i - itemp2: itemp1 * i - itemp3,j) = 1
          iMat(itemp1 * i - itemp4: itemp1 * i - itemp5,j) = 2
        enddo
      enddo
!
!   Compute joint probabilities
!
      do i = 1, 2 ** nobs
        prob_joint_I(i) = 0.d0
        do i_discrete_prior_beta = 1, n_discrete_prior_beta
          temp1 = 1.d0
          do j = 1, nobs
            if (iMat(i,j) == 1) then
              temp1 = temp1 * prob_Ij_eq_1_given_beta_m(j,i_discrete_prior_beta)
            elseif (iMat(i,j) == 2) then
              temp1 = temp1 * prob_Ij_eq_2_given_beta_m(j,i_discrete_prior_beta)
            endif
          enddo
          prob_joint_I(i) = prob_joint_I(i) + temp1 * discrete_prior_beta_prob(i_discrete_prior_beta)
        enddo
      enddo

9990  return
end subroutine Compute_iMat_and_initial_joint_probs_for_discrete_prior_1
!
!
subroutine Comp_min_exp_cost_given_all_previous_decisions_if_DoNotAskDM (n_alternatives_left_given_all_previous_decisions,           &
      prob_Ij_eq_1_given_previous_Is, dmin_expected_cost)
!
!**********************************************************************
!
!   Double precision subroutine to compute expected cost
!
!**********************************************************************

      use Shared_Constants
      use Shared_Data
      implicit none
!
!   Subroutine arguments
!
      integer :: n_alternatives_left_given_all_previous_decisions
      real (kind = double) :: dmin_expected_cost
      real (kind = double) :: prob_Ij_eq_1_given_previous_Is(n_alternatives_left_given_all_previous_decisions)
!
!   Local arguments
!
      integer :: i_alt
      real (kind = double) :: cost_admit_should_reject_times_prob_reject, cost_reject_should_admit_times_prob_admit
!
!   Begin calculations
! 
      dmin_expected_cost = 0.d0
      do i_alt = 1, n_alternatives_left_given_all_previous_decisions
        cost_admit_should_reject_times_prob_reject = cost_admit_should_reject * prob_Ij_eq_1_given_previous_Is(i_alt)
        cost_reject_should_admit_times_prob_admit = cost_reject_should_admit * (1.d0 - prob_Ij_eq_1_given_previous_Is(i_alt))
        dmin_expected_cost = dmin_expected_cost + min(cost_admit_should_reject_times_prob_reject, cost_reject_should_admit_times_prob_admit)
      enddo

      return
end subroutine Comp_min_exp_cost_given_all_previous_decisions_if_DoNotAskDM
!
!
subroutine Compute_min_exp_cost_for_jplace_kquestion_v5 (nquestions, k, j_left, ialt_ask_symm_mode,                                               &
  pr_Ieq1_given_prev_Is_after_k_questions_symm_mode, i_symm_mode_prob_begin, i_symm_mode_prob_end,                                                &
  exp_cost_stop_after_k_questions_Ieq1_symm_mode, exp_cost_stop_after_k_questions_Ieq2_symm_mode, i_symm_mode_begin, i_symm_mode_end)

!  *********************************************************************************************************************
!
!   Subroutine to compute minimum expected cost for a given j_left and k - Version 5 requires less array space
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      implicit none
!
!   Subroutine arguments
!
      integer :: nquestions, k, j_left, ispot_closest_to_pt5
      integer :: ivec(2)
      integer :: i_symm_mode_begin(nquestions), i_symm_mode_end(nquestions), i_symm_mode_prob_begin(0:nquestions), i_symm_mode_prob_end(0:nquestions)
      real (kind = double) :: exp_cost_stop_after_k_questions_Ieq1_symm_mode(2*(2**nquestions)),                                                       &
        exp_cost_stop_after_k_questions_Ieq2_symm_mode(2*(2**nquestions)),                                                                             &
        pr_Ieq1_given_prev_Is_after_k_questions_symm_mode(2*(2**nquestions))
!
!   Local arguments
!
      integer :: i, j, itemp, icount, I_branch, jplace, jplace_prev, itemp1, itemp2
      integer :: I_used(nobs), I_left_compact(nobs), ialt_ask_symm_mode(2*(2**nquestions))
      real (kind = double) :: denominator
      real (kind = double) :: wk1_nobs(nobs), pr_Ij_eq_1(nobs), prob_used(n_discrete_prior_beta), wk1_ndiscrete_beta(n_discrete_prior_beta)
!
!   Initialize variables
!
      jplace = 2 * j_left - 1
      jplace_prev = ceiling(jplace/2.d0)
      i_symm_mode_begin(k) = 2 ** (k - 1)
      i_symm_mode_end(k) = (2 ** k) - 1
      if (k < nquestions) i_symm_mode_begin(k+1) = 2 ** ((k + 1) - 1)
      i_symm_mode_prob_begin(k) = 2 ** k
      i_symm_mode_prob_end(k) = (2 ** (k +1)) -1
!
!   Compute I_used
!
      itemp = j_left
      I_used(k) = ialt_ask_symm_mode(i_symm_mode_begin(k)+(itemp-1))
      prob_used(1:n_discrete_prior_beta) = discrete_prior_beta_prob_store(1:n_discrete_prior_beta)
      itemp1 = j_left
      itemp2 = ceiling(j_left/2.d0)
      do j = k - 1, 1, -1
        I_used(j) = ialt_ask_symm_mode(i_symm_mode_begin(j)+(itemp2-1))
        if (itemp1 /= 2 * (itemp1 / 2)) then
          prob_used(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta) * prob_Ij_eq_1_given_beta_m(I_used(j),1:n_discrete_prior_beta)
        else
          prob_used(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta) * (1.d0 - prob_Ij_eq_1_given_beta_m(I_used(j),1:n_discrete_prior_beta))
        endif
        itemp1 = ceiling(itemp1/2.d0)
        itemp2 = ceiling(itemp2/2.d0)
      enddo
      icount = 1
      do i = 1, nobs
        if (any(I_used(1:k) == i)) then
          goto 100
        else
          I_left_compact(icount) = i
          icount = icount + 1
        endif
100     continue
      enddo
!
!   Begin calculations
!
      do I_branch = 1, 2
        if (I_branch == 1) then
          wk1_ndiscrete_beta(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta)                                                        &
            * prob_Ij_eq_1_given_beta_m(ialt_ask_symm_mode(i_symm_mode_begin(k)+(jplace_prev-1)),1:n_discrete_prior_beta)
          denominator = sum(wk1_ndiscrete_beta(1:n_discrete_prior_beta))
          pr_Ij_eq_1(1:nobs-k)                                                                                                                    &
            = matmul(prob_Ij_eq_1_given_beta_m(I_left_compact(1:nobs-k),1:n_discrete_prior_beta), wk1_ndiscrete_beta(1:n_discrete_prior_beta))    &
              / denominator
          wk1_nobs(1:nobs-k) = min(cost_admit_should_reject * pr_Ij_eq_1(1:nobs-k), cost_reject_should_admit * (1.d0 - pr_Ij_eq_1(1:nobs-k)))
          exp_cost_stop_after_k_questions_Ieq1_symm_mode(i_symm_mode_begin(k)+(j_left-1)) = sum(wk1_nobs(1:nobs-k)) + dble(k) * cost_ask_DM
        else
          wk1_ndiscrete_beta(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta)                                                        &
            * prob_Ij_eq_2_given_beta_m(ialt_ask_symm_mode(i_symm_mode_begin(k)+(jplace_prev-1)),1:n_discrete_prior_beta)
          denominator = sum(wk1_ndiscrete_beta(1:n_discrete_prior_beta))
          pr_Ij_eq_1(1:nobs-k)                                                                                                                    &
            = matmul(prob_Ij_eq_1_given_beta_m(I_left_compact(1:nobs-k),1:n_discrete_prior_beta), wk1_ndiscrete_beta(1:n_discrete_prior_beta))    &
              / denominator
          wk1_nobs(1:nobs-k) = min(cost_admit_should_reject * pr_Ij_eq_1(1:nobs-k), cost_reject_should_admit * (1.d0 - pr_Ij_eq_1(1:nobs-k)))
          exp_cost_stop_after_k_questions_Ieq2_symm_mode(i_symm_mode_begin(k)+(j_left-1)) = sum(wk1_nobs(1:nobs-k)) + dble(k) * cost_ask_DM
        endif
!
!   Select alternative closest to 0.5
!
        wk1_nobs(1:nobs-k) = min(cost_admit_should_reject * pr_Ij_eq_1(1:nobs-k), cost_reject_should_admit * (1.d0 - pr_Ij_eq_1(1:nobs-k)))
        ivec(1:1) = maxloc(wk1_nobs(1:nobs-k))
        ispot_closest_to_pt5 = ivec(1)
        pr_Ieq1_given_prev_Is_after_k_questions_symm_mode(i_symm_mode_prob_begin(k) + (jplace - 1)) = pr_Ij_eq_1(ispot_closest_to_pt5)
        if (k < nquestions) ialt_ask_symm_mode(i_symm_mode_begin(k+1)+(jplace-1)) = I_left_compact(ispot_closest_to_pt5)
!
!   Increase counter
!
        jplace = jplace + 1
      enddo

9990  return
end subroutine Compute_min_exp_cost_for_jplace_kquestion_v5
!
!
subroutine Compute_min_bounds_pt5_heuristic_general_n_and_nquestions_v2 (nquestions, exp_cost_stop_after_0questions, expected_cost_heuristic_path)

!  *******************************************************************************************************************************
!
!     Subroutine to compute minimum expected cost using the heuristic where alternative with marginal probability closest to 0.5 
!
!  *******************************************************************************************************************************

      use Shared_Constants
      use Shared_Data
      implicit none
!
!   Subroutine arguments
!
      integer :: nquestions
      real (kind = double) :: exp_cost_stop_after_0questions, expected_cost_heuristic_path
!
!   Local arguments
!
      integer :: j, k, jplace, j_left, ispot_closest_to_pt5, ntemp, i, icount1, icount2, itemp, icount
      integer :: ivec(2), I_used(nobs), I_left(nobs)
      integer :: i_odd(2**(nquestions-1)), i_even(2**(nquestions-1))
      real (kind = double) :: pr_Ij_eq_1(nobs), wk1_nobs(nobs), wk2_nobs(nobs)
      integer :: itemp1, itemp2
      integer :: ialt_ask_symm_mode(2*(2**nquestions))
      real (kind = double) :: denominator
      real (kind = double) :: prob_used(1:n_discrete_prior_beta), wk1_ndiscrete_beta(n_discrete_prior_beta)
      integer :: i_symm_mode_begin(nquestions), i_symm_mode_end(nquestions), i_symm_mode_prob_begin(0:nquestions), i_symm_mode_prob_end(0:nquestions)
      real (kind = double) :: exp_cost_stop_after_k_questions_Ieq1_symm_mode(2*(2**nquestions)),                                                       &
        exp_cost_stop_after_k_questions_Ieq2_symm_mode(2*(2**nquestions)),                                                                             &
        pr_Ieq1_given_prev_Is_after_k_questions_symm_mode(2*(2**nquestions)),                                                                          &
        dmin_exp_cost_roll_back_after_k_questions(2**nquestions), dmin_exp_cost_roll_back_after_k_questions_update(2**(nquestions-1))
!
!   Set i_odd and i_even for use in roll back calculations
!
      icount1 = 0
      icount2 = 0
      do i = 1, 2 ** nquestions
        if (2 * (i / 2) == i) then
          icount2 = icount2 + 1
          i_even(icount2) = i
        else
          icount1 = icount1 + 1
          i_odd(icount1) = i
        endif
      enddo
!
!   Compute expected cost of stopping immediately
!
      k = 0
      jplace = 1
      pr_Ij_eq_1(1:nobs) = matmul(prob_Ij_eq_1_given_beta_m(1:nobs,1:n_discrete_prior_beta), discrete_prior_beta_prob_store(1:n_discrete_prior_beta))
      exp_cost_stop_after_0questions = sum(min(cost_admit_should_reject * pr_Ij_eq_1(1:nobs), cost_reject_should_admit * (1.d0 - pr_Ij_eq_1(1:nobs))))
!
!   Select alternative closest to 0.5
!
      wk1_nobs(1:nobs-k) = min(cost_admit_should_reject * pr_Ij_eq_1(1:nobs-k), cost_reject_should_admit * (1.d0 - pr_Ij_eq_1(1:nobs-k)))
      ivec(1:1) = maxloc(wk1_nobs(1:nobs-k))
      ispot_closest_to_pt5 = ivec(1)
      i_symm_mode_prob_begin(k) = 2 ** k
      i_symm_mode_prob_end(k) = (2 ** (k +1)) -1
      pr_Ieq1_given_prev_Is_after_k_questions_symm_mode(i_symm_mode_prob_begin(k)) = pr_Ij_eq_1(ispot_closest_to_pt5)
      ialt_ask_symm_mode(1) = ispot_closest_to_pt5
!
!   Compute expected costs of stopping after k questions
!
      do k = 1, nquestions
        do j_left = 1, 2 ** (k - 1)
          call Compute_min_exp_cost_for_jplace_kquestion_v5 (nquestions, k, j_left, ialt_ask_symm_mode,                                            &
            pr_Ieq1_given_prev_Is_after_k_questions_symm_mode, i_symm_mode_prob_begin, i_symm_mode_prob_end,                                       &
            exp_cost_stop_after_k_questions_Ieq1_symm_mode, exp_cost_stop_after_k_questions_Ieq2_symm_mode, i_symm_mode_begin, i_symm_mode_end)
        enddo
      enddo
!
!   Compute upper bound - First compute starting values for roll back
!
      k = nquestions
      do j_left = 1, 2 ** (k - 1)
        itemp = j_left
        do j = k, 1, -1
          I_used(j) = ialt_ask_symm_mode(i_symm_mode_begin(j)+(itemp-1))
          itemp = ceiling(itemp/2.d0)
        enddo
        prob_used(1:n_discrete_prior_beta) = discrete_prior_beta_prob_store(1:n_discrete_prior_beta)
        itemp1 = 2 * j_left - 1
        itemp2 = ceiling((2 * j_left - 1)/2.d0)
        do j = k, 1, -1
          I_used(j) = ialt_ask_symm_mode(i_symm_mode_begin(j)+(itemp2-1))
          if (itemp1 /= 2 * (itemp1 / 2)) then
            prob_used(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta) * prob_Ij_eq_1_given_beta_m(I_used(j),1:n_discrete_prior_beta)
          else
            prob_used(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta) * (1.d0 - prob_Ij_eq_1_given_beta_m(I_used(j),1:n_discrete_prior_beta))
          endif
          itemp1 = ceiling(itemp1/2.d0)
          itemp2 = ceiling(itemp2/2.d0)
        enddo
        icount = 1
        do i = 1, nobs
          if (any(I_used(1:k) == i)) then
            goto 100
          else
            I_left(icount) = i
            icount = icount + 1
          endif
100       continue
        enddo
        wk1_ndiscrete_beta(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta)
        denominator = sum(wk1_ndiscrete_beta(1:n_discrete_prior_beta))
        pr_Ij_eq_1(1:nobs-k)                                                                                                                      &
          = matmul(prob_Ij_eq_1_given_beta_m(I_left(1:nobs-k),1:n_discrete_prior_beta), wk1_ndiscrete_beta(1:n_discrete_prior_beta))              &
            / denominator
        wk1_nobs(1:nobs-k) = min(cost_ask_DM, cost_admit_should_reject * pr_Ij_eq_1(1:nobs-k), cost_reject_should_admit * (1.d0 - pr_Ij_eq_1(1:nobs-k)))
        prob_used(1:n_discrete_prior_beta) = discrete_prior_beta_prob_store(1:n_discrete_prior_beta)
        itemp1 = 2 * j_left
        itemp2 = ceiling((2 * j_left)/2.d0)
        do j = k, 1, -1
          I_used(j) = ialt_ask_symm_mode(i_symm_mode_begin(j)+(itemp2-1))
          if (itemp1 /= 2 * (itemp1 / 2)) then
            prob_used(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta) * prob_Ij_eq_1_given_beta_m(I_used(j),1:n_discrete_prior_beta)
          else
            prob_used(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta) * (1.d0 - prob_Ij_eq_1_given_beta_m(I_used(j),1:n_discrete_prior_beta))
          endif
          itemp1 = ceiling(itemp1/2.d0)
          itemp2 = ceiling(itemp2/2.d0)
        enddo
        wk1_ndiscrete_beta(1:n_discrete_prior_beta) = prob_used(1:n_discrete_prior_beta)
        denominator = sum(wk1_ndiscrete_beta(1:n_discrete_prior_beta))
        pr_Ij_eq_1(1:nobs-k)                                                                                                                        &
          = matmul(prob_Ij_eq_1_given_beta_m(I_left(1:nobs-k),1:n_discrete_prior_beta), wk1_ndiscrete_beta(1:n_discrete_prior_beta))                &
            / denominator
        wk2_nobs(1:nobs-k) = min(cost_ask_DM, cost_admit_should_reject * pr_Ij_eq_1(1:nobs-k), cost_reject_should_admit * (1.d0 - pr_Ij_eq_1(1:nobs-k)))
        dmin_exp_cost_roll_back_after_k_questions(j_left)                                                                                           &
          = pr_Ieq1_given_prev_Is_after_k_questions_symm_mode(i_symm_mode_prob_begin(k-1) + (j_left - 1)) * sum(wk1_nobs(1:nobs-k))                 &
            + (1.d0 - pr_Ieq1_given_prev_Is_after_k_questions_symm_mode(i_symm_mode_prob_begin(k-1) + (j_left - 1))) * sum(wk2_nobs(1:nobs-k))      &    
            + dble(k) * cost_ask_DM
      enddo
!
!   Roll back expected costs
!
      do k = nquestions - 1, 1, -1
        ntemp = 2 ** (k - 1)
        dmin_exp_cost_roll_back_after_k_questions_update(1:ntemp)                                                                                 &
          = pr_Ieq1_given_prev_Is_after_k_questions_symm_mode(i_symm_mode_prob_begin(k-1):i_symm_mode_prob_end(k-1))                              &
            * min(exp_cost_stop_after_k_questions_Ieq1_symm_mode(i_symm_mode_begin(k):i_symm_mode_end(k)),                                        &
                  dmin_exp_cost_roll_back_after_k_questions(i_odd(1:ntemp/2)))                                                                    &
          + (1.d0 - pr_Ieq1_given_prev_Is_after_k_questions_symm_mode(i_symm_mode_prob_begin(k-1):i_symm_mode_prob_end(k-1)))                     &
            * min(exp_cost_stop_after_k_questions_Ieq2_symm_mode(i_symm_mode_begin(k):i_symm_mode_end(k)),                                        &
                  dmin_exp_cost_roll_back_after_k_questions(i_even(1:ntemp/2)))
        dmin_exp_cost_roll_back_after_k_questions(1:ntemp) = dmin_exp_cost_roll_back_after_k_questions_update(1:ntemp)
      enddo
      expected_cost_heuristic_path = dmin_exp_cost_roll_back_after_k_questions(1)

9990  return
end subroutine Compute_min_bounds_pt5_heuristic_general_n_and_nquestions_v2
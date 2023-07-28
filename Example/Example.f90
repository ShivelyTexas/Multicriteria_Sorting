!
!   Include statement for IMSL subroutines
!
      include 'link_fnl_shared.h' 

program Example

!  *********************************************************************************************************************
!
!     Application using the heuristic of choosing the alternative with marginal probability closest to 0.5
!
!  *********************************************************************************************************************

      use Shared_Constants
      use Shared_Data
!
!   IMSL subroutines used
!
      use qdagi_int; use svign_int; use rnset_int; use rnget_int; use rnnoa_int; use rnun_int; use zreal_int; use zbren_int; use anordf_int
      use rngam_int; use svrgn_int; use svrgp_int; use qdag_int; use linds_int; use blinf_int; use mxtxf_int
      implicit none
!
!   Subroutine argument
!
      real (kind = double) :: dummy_argument
!
!   Local arguments
!
      integer :: i, jreg, n_alternatives, nobs_original, nquestions, n_questions_initial, i_select, iobs00, n_beta1_previous, i_count_beta,   &
        i_discrete, i_diagonal, n_diagonal, i_beta1, n_beta1
      integer, allocatable, dimension (:) :: I_vector, I_left, I_decision, iperm, I_decision_wrong
      real (kind = double) :: cost_of_mistake, temp1, width, exp_cost_stop_after_0questions, expected_cost_heuristic_path,                    &
        d_joint_prob_for_I_irun_scalar, total_cost, dmin_x, dmax_x, total_prior_prob_for_beta_that_P_Deq2_at0_is_le_p0,                       &
        h_previous, h_low, h_up, h, dlog_density, sum_discrete_prior_beta_prob, det_Sigma_matrix, prob_Ij_eq_2_given_beta_at_xeq00
      real (kind = double) :: x_vec_at_xeq00(2), beta_vec(2)
      real (kind = double), allocatable, dimension (:) :: discrete_prior_beta_prob_original, d_joint_prob_given_beta, wk1_nobs, wk2_nobs
      real (kind = double), allocatable, dimension (:,:) :: X_matrix_original, X_matrix_print_at_end, X_transpose_X, Sigma_matrix,            &
        Sigma_matrix_inverse, Sigma_matrix_store, X_temp
      real (kind = double), external :: Determine_z_cutoff_given_beta_discrete_prior_nreg_ge_1_1
!
!   Set parameter values
!
      nobs_original = 73
      nreg = 2
      n_questions_initial = 20
      percentage_accept_global_for_prior_on_beta = 0.65d0
      cost_ask_DM = 0.30d0
      g_multiplier_for_p1 = 1.25d0
      g_multiplier_for_p0 = 0.077d0
      g_multiplier_for_p10 = 0.923d0
      g_multiplier_for_p01 = 0.769d0
      g_multiplier_for_mean_x_eq_1 = 2.0d0
      prob_threshold_for_xeq1_for_prior_beta_lower_bound = g_multiplier_for_p1 * percentage_accept_global_for_prior_on_beta
      prob_threshold_for_xeq0_for_prior_beta_upper_bound = g_multiplier_for_p0 * percentage_accept_global_for_prior_on_beta
      allocate (n_discrete_prior_beta_nreg_ge_1(nreg))
      n_discrete_prior_beta = 5000
      i_obs_other = int(nobs_original/2)
      cost_of_mistake = 1.d0
      cost_admit_should_reject = 1.0d0
      cost_reject_should_admit = 1.0d0
      allocate (discrete_prior_beta_prob(n_discrete_prior_beta), discrete_prior_beta_value_nreg_ge_1(n_discrete_prior_beta,nreg))
      nobs = nobs_original
      n_alternatives = nobs
!
!   Open file for output
!
      open (unit = 24, status = 'unknown', file = '00\Output.dat') 
!
!   Print input values
!
      write (*,10) nobs
      write (24,10) nobs
10    format (' nobs: ', i3)
      write (*,20) nreg
      write (24,20) nreg
20    format (' nreg: ', i3)
      write (*,30)  percentage_accept_global_for_prior_on_beta
      write (24,30) percentage_accept_global_for_prior_on_beta
30    format (' percentage_accept_global_for_prior_on_beta: ', f12.6)
      write (*,40)  g_multiplier_for_p0, g_multiplier_for_p10, g_multiplier_for_p01, g_multiplier_for_p1
      write (24,40) g_multiplier_for_p0, g_multiplier_for_p10, g_multiplier_for_p01, g_multiplier_for_p1
40    format (' g_multiplier_for_p0:  ', f12.6, /' g_multiplier_for_p10: ', f12.6,                                &
             /' g_multiplier_for_p01: ', f12.6, /' g_multiplier_for_p1:  ', f12.6)
      write (*,50)  n_questions_initial
      write (24,50) n_questions_initial
50    format (' n_questions_initial:', i4)
      write (*,60)  cost_ask_DM, cost_admit_should_reject, cost_reject_should_admit
      write (24,60) cost_ask_DM, cost_admit_should_reject, cost_reject_should_admit
60    format (' cost_ask_DM, cost_admit_should_reject, cost_reject_should_admit:', 3(1x, f8.4))
!
!   Allocate space for arrays
!
      allocate (I_vector(nobs), I_left(nobs), istudent_lower(nobs), istudent_upper(nobs), I_decision(nobs), iperm(nobs), I_decision_wrong(nobs))
      allocate (prob_Ij_eq_1(nobs), prob_Ii_eq_1(nobs), z_cutoff_for_each_beta(n_discrete_prior_beta),                                              &
        discrete_prior_beta_prob_original(n_discrete_prior_beta), d_joint_prob_given_beta(n_discrete_prior_beta), wk1_nobs(nobs),                   &
        discrete_prior_beta_prob_store(n_discrete_prior_beta), beta_vec_pass(nreg), wk2_nobs(nobs))
      allocate (prob_Ij_eq_1_given_beta_m(nobs,n_discrete_prior_beta), prob_Ij_eq_2_given_beta_m(nobs,n_discrete_prior_beta),                       &
        X_matrix(nobs,2), X_matrix_not_used_for_estimation(nobs,2), X_matrix_original(nobs,2), X_matrix_print_at_end(nobs,2),                       &
        X_transpose_X(nreg+1,nreg+1), Sigma_matrix(nreg+1,nreg+1), Sigma_matrix_store(nreg,nreg), Sigma_matrix_inverse(nreg,nreg), X_temp(nobs,3))
!
!   Open data file to use for plotting in R
!
      open (unit = 32, status = 'unknown', file = '00\I_vectors_X_matrix.dat')
!
!   Read data
!
      open (unit = 30, status = 'unknown', file = '00\Example.dat')
      do i = 1, nobs
        read (30,*) I_vector(i), X_matrix(i,1:2)
      enddo
      close (unit = 30)
      X_matrix_print_at_end(1:nobs,1:2) = X_matrix(1:nobs,1:2)
      do jreg = 1, nreg
        write (*,70)  jreg, X_matrix(1:nobs,jreg)
        write (24,70) jreg, X_matrix(1:nobs,jreg)
70      format (/' X_matrix(1:nobs,', i1, ') is:', 8(/10(1x, f12.6)))
      enddo
      write (*,80)  I_vector(1:nobs)
      write (24,80) I_vector(1:nobs)
80    format (/' I_vector(1:nobs) is:', 8(/10(1x, i12)))
!
!   Standardize columns of X
!
      do jreg = 1, nreg
        dmin_x = minval(X_matrix(1:nobs,jreg))
        dmax_x = maxval(X_matrix(1:nobs,jreg))
        X_matrix(1:nobs,jreg) = (X_matrix(1:nobs,jreg) - dmin_x) / (dmax_x - dmin_x)
      enddo
      X_matrix_original(1:nobs,1:nreg) = X_matrix(1:nobs,1:nreg)
!
!   Set percentage_pass and n_alternatives_pass valus
!
      percentage_pass = percentage_accept_global_for_prior_on_beta
      n_alternatives_pass = n_alternatives
!
!   Construct prior
!
!   First, construct Sigma matrix for prior
!
      X_temp(1:nobs,1) = 1.d0
      X_temp(1:nobs,2) = X_matrix(1:nobs,1)
      X_temp(1:nobs,3) = X_matrix(1:nobs,2)
      call mxtxf (X_temp(1:nobs,1:3), X_transpose_X(1:3,1:3))
      call linds (X_transpose_X(1:3,1:3), Sigma_matrix(1:3,1:3))
      Sigma_matrix_store(1:2,1:2) = Sigma_matrix(2:3,2:3)
!
!   Determine h in h*(X'X)_inverse
!
      total_prior_prob_for_beta_that_P_Deq2_at0_is_le_p0 = 0.001d0
      width = 0.25d0
      n_beta1 = 36
      n_beta1_previous = n_beta1; h_previous = 0.d0
      wk1_nobs(1:4) = 1.d10
      do i = 1, nobs
        wk2_nobs(1) = ((X_matrix(i,1) - 0.d0) ** 2) + ((X_matrix(i,2) - 0.d0) ** 2)
        if (wk2_nobs(1) < wk1_nobs(1)) then
          iobs00 = i
          wk1_nobs(1) = wk2_nobs(1)
        endif
      enddo
      x_vec_at_xeq00(1:nreg)  = X_matrix(iobs00,1:nreg)
100   h_low = 0.001d0; h_up = 100.d0
110   h = (h_low + h_up) / 2.d0
      Sigma_matrix(1:nreg,1:nreg) = h * Sigma_matrix_store(1:nreg,1:nreg)
      call linds (Sigma_matrix(1:nreg,1:nreg), Sigma_matrix_inverse(1:nreg,1:nreg))
      det_Sigma_matrix = Sigma_matrix(1,1) * Sigma_matrix(2,2) - Sigma_matrix(1,2) * Sigma_matrix(2,1)
!
!   Set grid
!
      n_discrete_prior_beta = 1
      n_diagonal = 0
      do i_beta1 = 0, n_beta1 - 1
        beta_vec(1) = i_beta1 * width
        beta_vec(2) = 0.d0
        discrete_prior_beta_value_nreg_ge_1(n_discrete_prior_beta,1:2) = beta_vec(1:2)
        n_discrete_prior_beta = n_discrete_prior_beta + 1
        do i_diagonal = 1, n_diagonal
          beta_vec(1) = beta_vec(1) - width
          beta_vec(2) = beta_vec(2) + width
          discrete_prior_beta_value_nreg_ge_1(n_discrete_prior_beta,1:2) = beta_vec(1:2)
          n_discrete_prior_beta = n_discrete_prior_beta + 1
        enddo
        n_diagonal = n_diagonal + 1
      enddo
      n_discrete_prior_beta = n_discrete_prior_beta - 1
!
!   Compute probabilities
!
      do i_discrete = 1, n_discrete_prior_beta
        beta_vec(1:2) = discrete_prior_beta_value_nreg_ge_1(i_discrete,1:2)
        dlog_density = -log(twopi) - 0.5d0 * log(det_Sigma_matrix) - 0.5d0 * blinf(Sigma_matrix_inverse(1:nreg,1:nreg), beta_vec(1:nreg), beta_vec(1:nreg))
        discrete_prior_beta_prob(i_discrete) = exp(dlog_density)
      enddo
      temp1 = sum(discrete_prior_beta_prob(1:n_discrete_prior_beta))
      discrete_prior_beta_prob(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta) / temp1
!
!   Compute probability I = 2 at x = (0, 0). If its less than 0.01, then add prior probability for beta
!
      i_count_beta = 0
      sum_discrete_prior_beta_prob = 0.d0
      i = 0
      do i_discrete = 1, n_discrete_prior_beta
        i = i + 1
        beta_vec(1:2) = discrete_prior_beta_value_nreg_ge_1(i_discrete,1:2)
        temp1 = Determine_z_cutoff_given_beta_discrete_prior_nreg_ge_1_1 (beta_vec(1:nreg))
        prob_Ij_eq_2_given_beta_at_xeq00 = 1.d0 - anordf(z_cutoff_pass - sum(x_vec_at_xeq00(1:nreg) * beta_vec(1:nreg)))
        if (prob_Ij_eq_2_given_beta_at_xeq00 < g_multiplier_for_p0  * percentage_accept_global_for_prior_on_beta) then
          sum_discrete_prior_beta_prob = sum_discrete_prior_beta_prob + discrete_prior_beta_prob(i)
          i_count_beta = i_count_beta + 1
        endif
      enddo
      if (i_count_beta < 3) then
        n_beta1 = n_beta1 + 1
        n_beta1_previous = n_beta1
        goto 100
      endif
      if (abs(sum_discrete_prior_beta_prob) < 1.d-8) then
        n_beta1_previous = n_beta1; h_previous = h
        n_beta1 = n_beta1 + 1
        goto 100
      endif
      if (abs(sum_discrete_prior_beta_prob - total_prior_prob_for_beta_that_P_Deq2_at0_is_le_p0) < 1.d-4) then
        if (abs(h - h_previous) < 1.d-4) then
          n_beta1 = n_beta1_previous; h = h_previous
          goto 120
        else          
          n_beta1_previous = n_beta1; h_previous = h
          n_beta1 = n_beta1 + 1
          goto 100
        endif
      elseif (sum_discrete_prior_beta_prob - total_prior_prob_for_beta_that_P_Deq2_at0_is_le_p0 < 0.d0) then
        h_low = h
        goto 110
      elseif (sum_discrete_prior_beta_prob - total_prior_prob_for_beta_that_P_Deq2_at0_is_le_p0 > 0.d0) then
        h_up = h
        goto 110
      endif
120   continue
!
!   Recalculate prior (recalculating saves some storage)
!
      Sigma_matrix(1:nreg,1:nreg) = h * Sigma_matrix_store(1:nreg,1:nreg)
      call linds (Sigma_matrix(1:nreg,1:nreg), Sigma_matrix_inverse(1:nreg,1:nreg))
      det_Sigma_matrix = Sigma_matrix(1,1) * Sigma_matrix(2,2) - Sigma_matrix(1,2) * Sigma_matrix(2,1)
      do i_discrete = 1, n_discrete_prior_beta
        beta_vec(1:2) = discrete_prior_beta_value_nreg_ge_1(i_discrete,1:2)
        dlog_density = -log(twopi) - 0.5d0 * log(det_Sigma_matrix) - 0.5d0 * blinf(Sigma_matrix_inverse(1:nreg,1:nreg), beta_vec(1:nreg), beta_vec(1:nreg))
        discrete_prior_beta_prob(i_discrete) = exp(dlog_density)
      enddo
      temp1 = sum(discrete_prior_beta_prob(1:n_discrete_prior_beta))
      discrete_prior_beta_prob(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta) / temp1
!
!   Print values for prior distribution for plotting in R
!
      open (unit = 34, status = 'unknown', file = '00\Prior_beta1_beta2_prob.dat')
      do i_discrete = 1, n_discrete_prior_beta
        write (34,130) discrete_prior_beta_value_nreg_ge_1(i_discrete,1:2), discrete_prior_beta_prob(i_discrete)
130     format (3(1x, f8.4))
      enddo
      close (unit = 34)
      X_matrix(1:nobs,1:nreg) = X_matrix_original(1:nobs,1:nreg)
      discrete_prior_beta_prob_original(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta)
      discrete_prior_beta_prob_store(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta)
!
!   Compute z_cutoff value for each beta value
!
      call Compute_z_cutoff_given_beta_m_nreg_ge_1_1 (dummy_argument)
!
!   Compute prob_Ij_eq_1_given_beta_m, istudent_upper and istudent_lower, and store
!
      call Compute_prob_Ij_eq_1_given_beta_m_values_nreg_ge_1_1 (dummy_argument)
      call Compute_lower_bd_for_full_tree_given_prior_for_beta_nreg_ge_1_1 (dummy_argument)
!
!   Compute minimum expected cost using the heuristic where alternative with marginal probability closest to 0.5 is selected
!
      do i = 1, nobs
        I_left(i) = i
      enddo
!
!   Ask DM about 00 alternatives
!
140   i_select = 1
      call Compute_prob_Ij_eq_1_1 (dummy_argument)
      do i = 1, nobs
        if (istudent_lower(i) == 0 .and. istudent_upper(i) == 0) then
          i_select = i
          goto 160
        endif
      enddo
      goto 180
160   continue
      write (*,170) I_left(i_select)
      write (24,170) I_left(i_select)
170   format (/' Remove observation #', i3)
!
!   Update prior with selected I
!
      if (I_vector(I_left(i_select)) == 1) then
        d_joint_prob_given_beta(1:n_discrete_prior_beta) = prob_Ij_eq_1_given_beta_m(i_select,1:n_discrete_prior_beta)
      else
        d_joint_prob_given_beta(1:n_discrete_prior_beta) = 1.d0 - prob_Ij_eq_1_given_beta_m(i_select,1:n_discrete_prior_beta)
      endif
      discrete_prior_beta_prob(1:n_discrete_prior_beta) = d_joint_prob_given_beta(1:n_discrete_prior_beta)                             &
        * discrete_prior_beta_prob_store(1:n_discrete_prior_beta)
      d_joint_prob_for_I_irun_scalar = sum(discrete_prior_beta_prob(1:n_discrete_prior_beta))
      discrete_prior_beta_prob(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta) / d_joint_prob_for_I_irun_scalar
      discrete_prior_beta_prob_store(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta)
!
!   Remove selected alternative and repeat heuristic calculation
!
      do i = i_select + 1, nobs
        prob_Ij_eq_1_given_beta_m(i-1,1:n_discrete_prior_beta) = prob_Ij_eq_1_given_beta_m(i,1:n_discrete_prior_beta)
        prob_Ij_eq_2_given_beta_m(i-1,1:n_discrete_prior_beta) = 1.d0 - prob_Ij_eq_1_given_beta_m(i-1,1:n_discrete_prior_beta)
        X_matrix(i-1,1:nreg) = X_matrix(i,1:nreg)
        istudent_lower(i-1) = istudent_lower(i)
        istudent_upper(i-1) = istudent_upper(i)
        I_left(i-1) = I_left(i)
      enddo
      nobs = nobs - 1
      goto 140
180   nquestions = min(n_questions_initial,nobs-1)
      call Compute_prob_Ij_eq_1_1 (dummy_argument)
      if (nobs == 1 * (nobs / 1)) print *, 'nobs =', nobs
      call Compute_min_bounds_pt5_heuristic_general_n_and_nquestions_v2 (nquestions, exp_cost_stop_after_0questions, expected_cost_heuristic_path)
      write (*,190) exp_cost_stop_after_0questions, expected_cost_heuristic_path
      write (24,190) exp_cost_stop_after_0questions, expected_cost_heuristic_path
190   format (/' exp_cost_stop_after_0questions: ', f12.6, /' expected_cost_heuristic_path:   ', f12.6)
!
!   If expected cost of stopping is less than expected cost of continuing, then the analyst places all remaining alternatives
!
      if (exp_cost_stop_after_0questions <= expected_cost_heuristic_path) goto 220
!
!   Otherwise, find the alternative with marginal probability closest to 0.5 that the heuristic selects and update the prior with the observed I
!
      call Compute_prob_Ij_eq_1_1 (dummy_argument)
      i_select = 1
      temp1 = abs(prob_Ij_eq_1(1) - 0.5d0)
      do i = 2, nobs
        if (abs(prob_Ij_eq_1(i) - 0.5d0) < temp1) then
          i_select = i
          temp1 = abs(prob_Ij_eq_1(i) - 0.5d0)
        endif
      enddo
      write (*,200) I_left(i_select)
      write (24,200) I_left(i_select)
200   format (/' Remove observation #', i3)
!
!   Update prior with selected I
!
      if (I_vector(I_left(i_select)) == 1) then
        d_joint_prob_given_beta(1:n_discrete_prior_beta) = prob_Ij_eq_1_given_beta_m(i_select,1:n_discrete_prior_beta)
      else
        d_joint_prob_given_beta(1:n_discrete_prior_beta) = 1.d0 - prob_Ij_eq_1_given_beta_m(i_select,1:n_discrete_prior_beta)
      endif
      discrete_prior_beta_prob(1:n_discrete_prior_beta) = d_joint_prob_given_beta(1:n_discrete_prior_beta)                             &
        * discrete_prior_beta_prob_store(1:n_discrete_prior_beta)
      d_joint_prob_for_I_irun_scalar = sum(discrete_prior_beta_prob(1:n_discrete_prior_beta))
      discrete_prior_beta_prob(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta) / d_joint_prob_for_I_irun_scalar
      discrete_prior_beta_prob_store(1:n_discrete_prior_beta) = discrete_prior_beta_prob(1:n_discrete_prior_beta)
!
!   Remove selected alternative and repeat heuristic calculation
!
      do i = i_select + 1, nobs
        prob_Ij_eq_1_given_beta_m(i-1,1:n_discrete_prior_beta) = prob_Ij_eq_1_given_beta_m(i,1:n_discrete_prior_beta)
        prob_Ij_eq_2_given_beta_m(i-1,1:n_discrete_prior_beta) = 1.d0 - prob_Ij_eq_1_given_beta_m(i-1,1:n_discrete_prior_beta)
        X_matrix(i-1,1:nreg) = X_matrix(i,1:nreg)
        I_left(i-1) = I_left(i)
      enddo
      nobs = nobs - 1
      goto 180
!
!   Compute total cost
!
220   total_cost = dble(nobs_original - nobs) * cost_ask_DM
      I_decision(1:nobs_original) = 99
!
!   Print values for posterior distribution for plotting in R
!
      open (unit = 34, status = 'unknown', file = '00\Post_finish_beta1_beta2_prob.dat')
      do i = 1, n_discrete_prior_beta
        write (34,230) discrete_prior_beta_value_nreg_ge_1(i,1), discrete_prior_beta_value_nreg_ge_1(i,2), discrete_prior_beta_prob(i)
230     format (3(1x, f12.6))
      enddo
      close (unit = 34)
!
!   Create decision vector and compute total cost
!
      do i = 1, nobs
        if (prob_Ij_eq_1(i) <= 1.d0 - prob_Ij_eq_1(i)) then
          I_decision(I_left(i)) = 2
        else
          I_decision(I_left(i)) = 1
        endif
        if (I_decision(I_left(i)) == 1 .and. I_vector(I_left(i)) == 2) then
          total_cost = total_cost + cost_reject_should_admit
        elseif (I_decision(I_left(i)) == 2 .and. I_vector(I_left(i)) == 1) then
          total_cost = total_cost + cost_admit_should_reject
        endif
      enddo
      write (*,240)  I_vector(1:nobs_original)
      write (24,240) I_vector(1:nobs_original)
240   format (/' Full I_vector is:', 5(/i9, 9(1x, i12)))
      write (*,250)  I_decision(1:nobs_original)
      write (24,250) I_decision(1:nobs_original)
250   format (' Full I_decision is:', 5(/i9, 9(1x, i12)))
      write (*,260)  total_cost
      write (24,260) total_cost
260   format (/' Total cost:', f8.2)
!
!   Compute I_decision_wrong vector with 8's for wrong decisions, 9's for Ask DM, and I_vector otherwise
!
      do i = 1, nobs_original
        if (I_decision(i) == 99) then
          I_decision(i) = 9
          I_decision_wrong(i) = 9
        else
          I_decision_wrong(i) = I_vector(i)
        endif
        if (I_vector(i) /= I_decision(i) .and. I_decision(i) /= 9) I_decision_wrong(i) = 8
      enddo
!
!   Write data to a file for plotting in R
!
      do i = 1, nobs_original
        write (32,270) X_matrix_print_at_end(i,1:2), I_vector(i), I_decision(i), I_decision_wrong(i)
270     format (2(1x, f12.6), 3(1x, i3))
      enddo

      stop
end program Example

#include "cdf.h"
#include "sequentialR.h"
using namespace std;
using namespace Rcpp ;

// [[Rcpp::depends(RcppEigen)]]

SequentialR::SequentialR(void) {
  cdf dist;
}

Eigen::VectorXd SequentialR::inverse_logistic(const Eigen::VectorXd& eta) const
{
  Eigen::VectorXd ordered_pi( eta.size() );
  double product = 1;
  for(int j=0; j<eta.size(); ++j)
  {
    ordered_pi[j] = product * Logistic::cdf_logit( eta(j) );
    product *= ( 1 - Logistic::cdf_logit( eta(j) ) );
  }
  return in_open_corner(ordered_pi);
}

Eigen::MatrixXd SequentialR::inverse_derivative_logistic(const Eigen::VectorXd& eta) const
{
  Eigen::MatrixXd M = Eigen::MatrixXd::Zero(eta.rows(),eta.rows());
  double product = 1.;
  for (int j=0; j < eta.rows(); ++j)
  {
    M(j,j) = Logistic::pdf_logit(eta(j)) * product;
    for (int i=0; i<j; ++i)
    { M(i,j) = - Logistic::pdf_logit(eta(i))  * std::max(1e-10, std::min(Logistic::cdf_logit(eta(j)), 1-1e-6)) * product / std::max(1e-10, std::min( 1-Logistic::cdf_logit(eta(i)), 1-1e-6)); }
    product *= std::max(1e-10, std::min( 1-Logistic::cdf_logit(eta(j)), 1-1e-6));
  }
  return M;
}

Eigen::VectorXd SequentialR::inverse_normal(const Eigen::VectorXd& eta) const
{
  Eigen::VectorXd ordered_pi( eta.size() );
  double product = 1;
  for(int j=0; j<eta.size(); ++j)
  {
    ordered_pi[j] = product * cdf_normal( eta(j) );
    product *= ( 1 - cdf_normal( eta(j) ) );
  }
  return in_open_corner(ordered_pi);
}

Eigen::MatrixXd SequentialR::inverse_derivative_normal(const Eigen::VectorXd& eta) const
{
  Eigen::MatrixXd M = Eigen::MatrixXd::Zero(eta.rows(),eta.rows());
  double product = 1.;
  for (int j=0; j < eta.rows(); ++j)
  {
    M(j,j) = pdf_normal(eta(j)) * product;
    for (int i=0; i<j; ++i)
    { M(i,j) = - pdf_normal(eta(i))  * std::max(1e-10, std::min(cdf_normal(eta(j)), 1-1e-6)) * product / std::max(1e-10, std::min( 1-cdf_normal(eta(i)), 1-1e-6)); }
    product *= std::max(1e-10, std::min( 1-cdf_normal(eta(j)), 1-1e-6));
  }
  return M;
}

Eigen::VectorXd SequentialR::inverse_cauchy(const Eigen::VectorXd& eta) const
{
  Eigen::VectorXd ordered_pi( eta.size() );
  double product = 1;
  for(int j=0; j<eta.size(); ++j)
  {
    ordered_pi[j] = product * cdf_cauchy( eta(j) );
    product *= ( 1 - cdf_cauchy( eta(j) ) );
  }
  return in_open_corner(ordered_pi);
}

Eigen::MatrixXd SequentialR::inverse_derivative_cauchy(const Eigen::VectorXd& eta) const
{
  Eigen::MatrixXd M = Eigen::MatrixXd::Zero(eta.rows(),eta.rows());
  double product = 1.;
  for (int j=0; j < eta.rows(); ++j)
  {
    M(j,j) = pdf_cauchy(eta(j)) * product;
    for (int i=0; i<j; ++i)
    { M(i,j) = - pdf_cauchy(eta(i))  * std::max(1e-10, std::min(cdf_cauchy(eta(j)), 1-1e-6)) * product / std::max(1e-10, std::min( 1-cdf_cauchy(eta(i)), 1-1e-6)); }
    product *= std::max(1e-10, std::min( 1-cdf_cauchy(eta(j)), 1-1e-6));
  }
  return M;
}

Eigen::VectorXd SequentialR::inverse_gompertz(const Eigen::VectorXd& eta) const
{
  Eigen::VectorXd ordered_pi( eta.size() );
  double product = 1;
  for(int j=0; j<eta.size(); ++j)
  {
    ordered_pi[j] = product * cdf_gompertz( eta(j) );
    product *= ( 1 - cdf_gompertz( eta(j) ) );
  }
  return in_open_corner(ordered_pi);
}

Eigen::MatrixXd SequentialR::inverse_derivative_gompertz(const Eigen::VectorXd& eta) const
{
  Eigen::MatrixXd M = Eigen::MatrixXd::Zero(eta.rows(),eta.rows());
  double product = 1.;
  for (int j=0; j < eta.rows(); ++j)
  {
    M(j,j) = pdf_gompertz(eta(j)) * product;
    for (int i=0; i<j; ++i)
    { M(i,j) = - pdf_gompertz(eta(i))  * std::max(1e-10, std::min(cdf_gompertz(eta(j)), 1-1e-6)) * product / std::max(1e-10, std::min( 1-cdf_gompertz(eta(i)), 1-1e-6)); }
    product *= std::max(1e-10, std::min( 1-cdf_gompertz(eta(j)), 1-1e-6));
  }
  return M;
}

Eigen::VectorXd SequentialR::inverse_gumbel(const Eigen::VectorXd& eta) const
{
  Eigen::VectorXd ordered_pi( eta.size() );
  double product = 1;
  for(int j=0; j<eta.size(); ++j)
  {
    ordered_pi[j] = product * cdf_gumbel( eta(j) );
    product *= ( 1 - cdf_gumbel( eta(j) ) );
  }
  return in_open_corner(ordered_pi);
}

Eigen::MatrixXd SequentialR::inverse_derivative_gumbel(const Eigen::VectorXd& eta) const
{
  Eigen::MatrixXd M = Eigen::MatrixXd::Zero(eta.rows(),eta.rows());
  double product = 1.;
  for (int j=0; j < eta.rows(); ++j)
  {
    M(j,j) = pdf_gumbel(eta(j)) * product;
    for (int i=0; i<j; ++i)
    { M(i,j) = - pdf_gumbel(eta(i))  * std::max(1e-10, std::min(cdf_gumbel(eta(j)), 1-1e-6)) * product / std::max(1e-10, std::min( 1-cdf_gumbel(eta(i)), 1-1e-6)); }
    product *= std::max(1e-10, std::min( 1-cdf_gumbel(eta(j)), 1-1e-6));
  }
  return M;
}

Eigen::VectorXd SequentialR::inverse_student(const Eigen::VectorXd& eta, const double& freedom_degrees) const
{
  Eigen::VectorXd ordered_pi( eta.size() );
  double product = 1;
  for(int j=0; j<eta.size(); ++j)
  {
    ordered_pi[j] = product * cdf_student( eta(j) ,freedom_degrees);
    product *= ( 1 - cdf_student( eta(j) ,freedom_degrees) );
  }
  return in_open_corner(ordered_pi);
}

Eigen::MatrixXd SequentialR::inverse_derivative_student(const Eigen::VectorXd& eta, const double& freedom_degrees) const
{
  Eigen::MatrixXd M = Eigen::MatrixXd::Zero(eta.rows(),eta.rows());
  double product = 1.;
  for (int j=0; j < eta.rows(); ++j)
  {
    M(j,j) = pdf_student(eta(j),freedom_degrees) * product;
    for (int i=0; i<j; ++i)
    { M(i,j) = - pdf_student(eta(i),freedom_degrees)  * std::max(1e-10, std::min(cdf_student(eta(j),freedom_degrees), 1-1e-6)) * product / std::max(1e-10, std::min( 1-cdf_student(eta(i),freedom_degrees), 1-1e-6)); }
    product *= std::max(1e-10, std::min( 1-cdf_student(eta(j),freedom_degrees), 1-1e-6));
  }
  return M;
}

Eigen::VectorXd SequentialR::inverse_laplace(const Eigen::VectorXd& eta) const
{
  Eigen::VectorXd ordered_pi( eta.size() );
  double product = 1;
  for(int j=0; j<eta.size(); ++j)
  {
    ordered_pi[j] = product * Laplace::cdf_laplace( eta(j) );
    product *= ( 1 - Laplace::cdf_laplace( eta(j) ) );
  }
  return in_open_corner(ordered_pi);
}

Eigen::MatrixXd SequentialR::inverse_derivative_laplace(const Eigen::VectorXd& eta) const
{
  Eigen::MatrixXd M = Eigen::MatrixXd::Zero(eta.rows(),eta.rows());
  double product = 1.;
  for (int j=0; j < eta.rows(); ++j)
  {
    M(j,j) = Laplace::pdf_laplace(eta(j)) * product;
    for (int i=0; i<j; ++i)
    { M(i,j) = - Laplace::pdf_laplace(eta(i))  * std::max(1e-10, std::min(Laplace::cdf_laplace(eta(j)), 1-1e-6)) * product / std::max(1e-10, std::min( 1-Laplace::cdf_laplace(eta(i)), 1-1e-6)); }
    product *= std::max(1e-10, std::min( 1-Laplace::cdf_laplace(eta(j)), 1-1e-6));
  }
  return M;
}

Eigen::VectorXd SequentialR::inverse_noncentralt(const Eigen::VectorXd& eta, const double& freedom_degrees,const double& mu) const
{
  Eigen::VectorXd ordered_pi( eta.size() );
  double product = 1;
  for(int j=0; j<eta.size(); ++j)
  {
    ordered_pi[j] = product * cdf_non_central_t( eta(j) ,freedom_degrees,mu);
    product *= ( 1 - cdf_non_central_t( eta(j) ,freedom_degrees,mu) );
  }
  return in_open_corner(ordered_pi);
}

Eigen::MatrixXd SequentialR::inverse_derivative_noncentralt(const Eigen::VectorXd& eta, const double& freedom_degrees,const double& mu) const
{
  Eigen::MatrixXd M = Eigen::MatrixXd::Zero(eta.rows(),eta.rows());
  double product = 1.;
  for (int j=0; j < eta.rows(); ++j)
  {
    M(j,j) = pdf_non_central_t(eta(j),freedom_degrees,mu) * product;
    for (int i=0; i<j; ++i)
    { M(i,j) = - pdf_non_central_t(eta(i),freedom_degrees,mu)  * std::max(1e-10, std::min(cdf_non_central_t(eta(j),freedom_degrees,mu), 1-1e-6)) * product / std::max(1e-10, std::min( 1-cdf_non_central_t(eta(i),freedom_degrees,mu), 1-1e-6)); }
    product *= std::max(1e-10, std::min( 1-cdf_non_central_t(eta(j),freedom_degrees,mu), 1-1e-6));
  }
  return M;
}

// cdf dist_seq;
//
// // [[Rcpp::export]]
// List GLMseq(Formula formula,
//             CharacterVector categories_order,
//             CharacterVector parallel,
//             DataFrame data,
//             std::string cdf,
//             double freedom_degrees){
//
//   const int N = data.nrows() ; // Number of observations
//
//   List Full_M = dist_seq.All_pre_data_or(formula, data,
//                                       categories_order, parallel);
//
//   Eigen::MatrixXd Y_init = Full_M["Response_EXT"];
//   Eigen::MatrixXd X_EXT = Full_M["Design_Matrix"];
//   CharacterVector levs1 = Full_M["Levels"];
//   CharacterVector explanatory_complete = Full_M["Complete_effects"];
//
//   int P_c = explanatory_complete.length();
//   int P_p = 0;
//   if(parallel[0] != "NA"){P_p = parallel.length();}
//   int P =  P_c +  P_p ; // Number of explanatory variables without intercept
//
//   int Q = Y_init.cols();
//   int K = Q + 1;
//
//   // // // Beta initialization with zeros
//   Eigen::MatrixXd BETA;
//   BETA = Eigen::MatrixXd::Zero(X_EXT.cols(),1);
//   //
//   int iteration = 0;
//   // double check_tutz = 1.0;
//   double Stop_criteria = 1.0;
//   Eigen::MatrixXd X_M_i ;
//   Eigen::VectorXd Y_M_i ;
//   Eigen::VectorXd eta ;
//   Eigen::VectorXd pi ;
//   Eigen::MatrixXd D ;
//   Eigen::MatrixXd Cov_i ;
//   Eigen::MatrixXd W_in ;
//   Eigen::MatrixXd Score_i_2 ;
//   Eigen::MatrixXd F_i_2 ;
//   Eigen::VectorXd LogLikIter;
//   LogLikIter = Eigen::MatrixXd::Zero(1,1) ;
//   Eigen::MatrixXd cov_beta;
//   Eigen::VectorXd Std_Error;
//   double LogLik;
//   Eigen::MatrixXd pi_ma(N, K);
//   Eigen::MatrixXd F_i_final = Eigen::MatrixXd::Zero(BETA.rows(), BETA.rows());
//
//   // for (int iteration=1; iteration < 18; iteration++){
//   // while (check_tutz > 0.0001){
//   double epsilon = 0.0001 ;
//   while (Stop_criteria >( epsilon / N)){
//
//     Eigen::MatrixXd Score_i = Eigen::MatrixXd::Zero(BETA.rows(),1);
//     Eigen::MatrixXd F_i = Eigen::MatrixXd::Zero(BETA.rows(), BETA.rows());
//     LogLik = 0.;
//
//     // Loop by subject
//     for (int i=0; i < N; i++){
//       // Block of size (p,q), starting at (i,j): matrix.block(i,j,p,q);
//       X_M_i = X_EXT.block(i*Q , 0 , Q , X_EXT.cols());
//       Y_M_i = Y_init.row(i);
//       eta = X_M_i * BETA;
//
//       SequentialR seq;
//       // Vector pi depends on selected cdf
//       if(cdf == "logistic"){
//         pi = seq.inverse_logistic(eta);
//         D = seq.inverse_derivative_logistic(eta);
//       }else if(cdf == "normal"){
//         pi = seq.inverse_normal(eta);
//         D = seq.inverse_derivative_normal(eta);
//       }else if(cdf == "cauchy"){
//         pi = seq.inverse_cauchy(eta);
//         D = seq.inverse_derivative_cauchy(eta);
//       }else if(cdf == "gompertz"){
//         pi = seq.inverse_gompertz(eta);
//         D = seq.inverse_derivative_gompertz(eta);
//       }
//
//       Cov_i = Eigen::MatrixXd(pi.asDiagonal()) - (pi*pi.transpose());
//       W_in = D * Cov_i.inverse();
//       Score_i_2 = X_M_i.transpose() * W_in * (Y_M_i - pi);
//       Score_i = Score_i + Score_i_2;
//       F_i_2 = X_M_i.transpose() * (W_in) * (D.transpose() * X_M_i);
//       F_i = F_i + F_i_2;
//       LogLik = LogLik + (Y_M_i.transpose().eval()*Eigen::VectorXd(pi.array().log())) + ( (1 - Y_M_i.sum()) * std::log(1 - pi.sum()) );
//
//       pi_ma.row(i) = pi.transpose();
//
//     }
//
//     Eigen::VectorXd Ones1 = Eigen::VectorXd::Ones(pi_ma.rows());
//     pi_ma.col(Q) = Ones1 - pi_ma.rowwise().sum() ;
//
//     LogLikIter.conservativeResize(iteration+2, 1);
//     LogLikIter(iteration+1) = LogLik;
//     Stop_criteria = (abs(LogLikIter(iteration+1) - LogLikIter(iteration))) / (epsilon + (abs(LogLikIter(iteration+1)))) ;
//     Eigen::VectorXd beta_old = BETA;
//     BETA = BETA + (F_i.inverse() * Score_i);
//     // check_tutz = ((BETA - beta_old).norm())/(beta_old.norm()+check_tutz);
//     iteration = iteration + 1;
//     F_i_final = F_i;
//
//   }
//
//   cov_beta = F_i_final.inverse();
//   Std_Error = cov_beta.diagonal();
//   Std_Error = Std_Error.array().sqrt() ;
//
//   std::vector<std::string> text=as<std::vector<std::string>>(explanatory_complete);
//   std::vector<std::string> level_text=as<std::vector<std::string>>(categories_order);
//   StringVector names(Q*P_c + P_p);
//   if(P_c > 0){
//     for(int var = 0 ; var < explanatory_complete.size() ; var++){
//       for(int cat = 0 ; cat < Q ; cat++){
//         names[(Q*var) + cat] = dist_seq.concatenate(text[var], level_text[cat]);
//       }
//     }
//   }
//   if(P_p > 0){
//     for(int var_p = 0 ; var_p < parallel.size() ; var_p++){
//       names[(Q*P_c) + var_p] = parallel[var_p];
//     }
//   }
//
//   // TO NAMED THE RESULT BETAS
//   NumericMatrix coef = wrap(BETA);
//   rownames(coef) = names;
//
//   // AIC
//   double AIC = (-2*LogLik) + (2 *coef.length());
//
//   // AIC
//   double BIC = (-2*LogLik) + (coef.length() * log(N) );
//
//   int df = (N*Q) - coef.length();
//
//   Eigen::MatrixXd predict_glmcated = X_EXT * BETA;
//
//   Eigen::VectorXd Ones2 = Eigen::VectorXd::Ones(Y_init.rows());
//   Eigen::VectorXd vex1 = (Y_init.rowwise().sum()) ;
//   Y_init.conservativeResize( Y_init.rows(), K);
//   Y_init.col(Q) = (vex1 - Ones2).array().abs() ;
//
//   Eigen::MatrixXd residuals = Y_init - pi_ma;
//
//   Eigen::VectorXd pi_ma_vec(Eigen::Map<Eigen::VectorXd>(pi_ma.data(), pi_ma.cols()*pi_ma.rows()));
//   Eigen::VectorXd Y_init_vec(Eigen::Map<Eigen::VectorXd>(Y_init.data(), Y_init.cols()*Y_init.rows()));
//
//   Eigen::VectorXd div_arr = Y_init_vec.array() / pi_ma_vec.array();
//
//   Eigen::VectorXd dev_r(Y_init.rows());
//
//   int el_1 = 0;
//
//   for (int element = 0 ; element < div_arr.size() ;  element++){
//     if (div_arr[element] != 0){
//       dev_r[el_1] = div_arr[element];
//       el_1 = el_1 +1 ;
//     }
//   }
//
//   Eigen::ArrayXd dev_log = dev_r.array().log();
//
//   double deviance = dev_log.sum();
//   deviance = -2*deviance;
//
//   return List::create(
//     // Named("Nb. iterations") = iteration-1 ,
//     Named("coefficients") = coef,
//     Named("AIC") = AIC,
//     Named("BIC") = BIC,
//     // Named("cov_beta") = cov_beta,
//     Named("stderr") = Std_Error,
//     Rcpp::Named("df") = df,
//     Rcpp::Named("predict_glmcated") = predict_glmcated,
//     Rcpp::Named("fitted") = pi_ma,
//     Rcpp::Named("pi_ma_vec") = pi_ma_vec,
//     Rcpp::Named("Y_init_vec") = Y_init_vec,
//     Rcpp::Named("dev_log") = dev_log,
//     Rcpp::Named("deviance") = deviance,
//     Rcpp::Named("residuals") = residuals,
//     Named("LogLikelihood") = LogLik
//   );
//
// }
//
// RCPP_MODULE(sequentialmodule){
//   Rcpp::function("GLMseq", &GLMseq,
//                  List::create(_["formula"] = R_NaN,
//                               _["categories_order"] = CharacterVector::create( "A", NA_STRING),
//                               _["parallel"] = CharacterVector::create(NA_STRING),
//                               _["data"] = NumericVector::create( 1, NA_REAL, R_NaN, R_PosInf, R_NegInf),
//                               _["cdf"] = "a",
//                               _["freedom_degrees"] = 1.0),
//                               "Sequential model");
//   Rcpp::class_<SequentialR>("SequentialR")
//   .constructor()
//   // .method( "GLMseq", &SequentialR::GLMseq )
//   ;
// }




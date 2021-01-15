// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// Discrete_CM
List Discrete_CM(Formula formula, String case_id, String alternatives, CharacterVector reference, CharacterVector alternative_specific, DataFrame data, std::string distribution, double freedom_degrees);
RcppExport SEXP _GLMcat_Discrete_CM(SEXP formulaSEXP, SEXP case_idSEXP, SEXP alternativesSEXP, SEXP referenceSEXP, SEXP alternative_specificSEXP, SEXP dataSEXP, SEXP distributionSEXP, SEXP freedom_degreesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Formula >::type formula(formulaSEXP);
    Rcpp::traits::input_parameter< String >::type case_id(case_idSEXP);
    Rcpp::traits::input_parameter< String >::type alternatives(alternativesSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type reference(referenceSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type alternative_specific(alternative_specificSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type distribution(distributionSEXP);
    Rcpp::traits::input_parameter< double >::type freedom_degrees(freedom_degreesSEXP);
    rcpp_result_gen = Rcpp::wrap(Discrete_CM(formula, case_id, alternatives, reference, alternative_specific, data, distribution, freedom_degrees));
    return rcpp_result_gen;
END_RCPP
}
// GLMcat
List GLMcat(Formula formula, DataFrame data, std::string ratio, std::string distribution, CharacterVector proportional, CharacterVector categories_order, CharacterVector ref_category, double freedom_degrees, std::string threshold, NumericVector beta_init);
RcppExport SEXP _GLMcat_GLMcat(SEXP formulaSEXP, SEXP dataSEXP, SEXP ratioSEXP, SEXP distributionSEXP, SEXP proportionalSEXP, SEXP categories_orderSEXP, SEXP ref_categorySEXP, SEXP freedom_degreesSEXP, SEXP thresholdSEXP, SEXP beta_initSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Formula >::type formula(formulaSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< std::string >::type ratio(ratioSEXP);
    Rcpp::traits::input_parameter< std::string >::type distribution(distributionSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type proportional(proportionalSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type categories_order(categories_orderSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type ref_category(ref_categorySEXP);
    Rcpp::traits::input_parameter< double >::type freedom_degrees(freedom_degreesSEXP);
    Rcpp::traits::input_parameter< std::string >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type beta_init(beta_initSEXP);
    rcpp_result_gen = Rcpp::wrap(GLMcat(formula, data, ratio, distribution, proportional, categories_order, ref_category, freedom_degrees, threshold, beta_init));
    return rcpp_result_gen;
END_RCPP
}
// predict_glmcat
NumericMatrix predict_glmcat(List model_object, DataFrame data, String type);
RcppExport SEXP _GLMcat_predict_glmcat(SEXP model_objectSEXP, SEXP dataSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type model_object(model_objectSEXP);
    Rcpp::traits::input_parameter< DataFrame >::type data(dataSEXP);
    Rcpp::traits::input_parameter< String >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(predict_glmcat(model_object, data, type));
    return rcpp_result_gen;
END_RCPP
}
// Cat_ref1
List Cat_ref1(CharacterVector categories_order, RObject response_categories);
RcppExport SEXP _GLMcat_Cat_ref1(SEXP categories_orderSEXP, SEXP response_categoriesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type categories_order(categories_orderSEXP);
    Rcpp::traits::input_parameter< RObject >::type response_categories(response_categoriesSEXP);
    rcpp_result_gen = Rcpp::wrap(Cat_ref1(categories_order, response_categories));
    return rcpp_result_gen;
END_RCPP
}

RcppExport SEXP _rcpp_module_boot_discretemodule();
RcppExport SEXP _rcpp_module_boot_GLMcatmodule();

static const R_CallMethodDef CallEntries[] = {
    {"_GLMcat_Discrete_CM", (DL_FUNC) &_GLMcat_Discrete_CM, 8},
    {"_GLMcat_GLMcat", (DL_FUNC) &_GLMcat_GLMcat, 10},
    {"_GLMcat_predict_glmcat", (DL_FUNC) &_GLMcat_predict_glmcat, 3},
    {"_GLMcat_Cat_ref1", (DL_FUNC) &_GLMcat_Cat_ref1, 2},
    {"_rcpp_module_boot_discretemodule", (DL_FUNC) &_rcpp_module_boot_discretemodule, 0},
    {"_rcpp_module_boot_GLMcatmodule", (DL_FUNC) &_rcpp_module_boot_GLMcatmodule, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_GLMcat(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <Rversion.h>


void R_init_gnlm(DllInfo *dll)
{
	R_registerRoutines(dll, NULL, NULL, NULL, NULL);
	R_useDynamicSymbols(dll, FALSE);
}

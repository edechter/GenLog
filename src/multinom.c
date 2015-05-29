/* multinom.c
 * Author: Eyal Dechter
   Date: 5/28/2015

   c implementation of multinomial loglikelihood function for use in swi prolog. 

   To compile on my machine: 
   >> gcc -I/usr/local/Cellar/swi-prolog/HEAD/libexec/lib/swipl-7.3.1/include \\
   -fPIC -c -v multinom.c
   >> gcc -v -shared -W1 -o libmultinom.dylib multinom.o \\
   -L/usr/local/Cellar/swi-prolog/HEAD/libexec/lib/swipl-7.3.1/lib/x86_64-darwin13.2.0 \\
   -lswipl

 */

#include <SWI-Prolog.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <assert.h>
#include <math.h>
#include <gsl/gsl_sf_gamma.h>

/* Code copied from gsl/randist/multinomial.c. But here, we do not
    normalize the probability distribution. 
 */

double
multinomial_lnpdf (const size_t K,
                   const double p[], 
                   const unsigned int n[])
{
  size_t k;
  unsigned int N = 0;
  double log_pdf = 0.0;

  for (k = 0; k < K; k++)
    {
      N += n[k];
    }

  for (k = 0; k < K; k++)
    {
      /* Handle case where n[k]==0 and p[k]==0 */

      if (n[k] > 0) 
        {
          log_pdf += log (p[k]) * n[k];
        }
    }

  return log_pdf;
}

foreign_t
pl_multinomial_lnpdf(term_t N_term, term_t P_term, term_t LogP_term)
{ 
  term_t ignore = PL_new_term_ref();

  size_t k;
  if (!(PL_skip_list(N_term, ignore, &k) == PL_LIST))
    PL_fail;

  unsigned int n[k];
  double p[k];
  unsigned int N = 0;

  term_t N_term_copy = PL_copy_term_ref(N_term);
  term_t P_term_copy = PL_copy_term_ref(P_term);
  term_t nk_term     = PL_new_term_ref();
  term_t pk_term     = PL_new_term_ref();

  unsigned int i = 0;
  while (PL_get_list(N_term_copy, nk_term, N_term_copy)) {
    int x;
    if (PL_get_integer(nk_term, &x))
      {
        n[i] = x;
        N += x;
      }
    else
      PL_fail;

    i++;
  }


  i = 0;
  while (PL_get_list(P_term_copy, pk_term, P_term_copy)) {
    double x;
    if (PL_get_float(pk_term, &x))
      {
        p[i] = x;
      }
    else
      PL_fail;
    
    i++;
  }

  double logP;
  logP = multinomial_lnpdf(k, p, n);

  int rval;
  rval = PL_unify_float(LogP_term, logP);

  return rval;

}

install_t
install()
{ PL_register_foreign("multinomial_lnpdf", 3, pl_multinomial_lnpdf, 0);

}

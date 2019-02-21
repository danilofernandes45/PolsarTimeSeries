#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>


int indx;

//Given an array, it returns all possible permutations of it in another array
void permute(int* element, int init, int end, int* array) { 
    
    int i, aux;

    if(init == end){
        for(i = 0; i <= end; i++){ 
            array[indx] = element[i];
            indx++;
        }   
    }

    else{ 

        for (i = init; i <= end; i++) { 
            aux = element[init];
            element[init] = element[i];
            element[i] = aux;
            permute(element, init + 1, end, array); 
            aux = element[init];
            element[init] = element[i];
            element[i] = aux;
        } 
    } 
}

//Given a pattern, searches for its symbolization
int equals(int* pattern, int*symbols, int size){

    int i, j, aux = 0;
    for(i = 0; i < size; i++){
        if(pattern[i] == symbols[i]){
            aux++;
        }
    }
    if(aux == size){
        return 1;
    }
    else{
        return 0 ;
    }
}

SEXP BandtPompe(SEXP Relements, SEXP Rdimension, SEXP Rdelay, SEXP Rseriesize){

    int i, j, k = 0, dimFat = 1, n, aux = 0, dimension, delay, seriesize;
    double* elements_aux;
    SEXP Rprobability;

    Rdimension = coerceVector(Rdimension, INTSXP);
    Rdelay = coerceVector(Rdelay, INTSXP);
    Relements = coerceVector(Relements, REALSXP);
    Rseriesize = coerceVector(Rseriesize, INTSXP);

    dimension = INTEGER(Rdimension)[0];
    delay = INTEGER(Rdelay)[0];
    seriesize = INTEGER(Rseriesize)[0];
    n = dimension;    

    elements_aux = REAL(Relements);

    while(1 < n)
    {
        dimFat = n*dimFat;
        n--;
    }

    printf(".\n");

    //allocates space for the bidimensional arrays(symbols, elements, patterns)
    int** symbols = (int**) malloc(dimFat * sizeof(int*));
    for(i = 0; i < dimFat; i++){
        symbols[i] = (int*) malloc(dimension * sizeof(int));
    }

    int **patterns = (int**) malloc((seriesize - (dimension - 1)*delay) * sizeof(int*));
    double **elements = (double**) malloc((seriesize - (dimension - 1)*delay) * sizeof(double*));
    for(i = 0; i < (seriesize - (dimension - 1)*delay); i++){
        patterns[i] = (int*) malloc(dimension * sizeof(int));
        elements[i] = (double*) malloc(dimension * sizeof(double));
    }

    //filling the bidimensional arrays
    int* initial_pattern = (int*) malloc(dimension * sizeof(int));
    int* permutations = (int*) malloc((dimFat*dimension) * sizeof(int));

    for(i = 0; i < dimension; i++){
        initial_pattern[i] = i;
    }

    for(i = 0; i < (seriesize - (dimension - 1)*delay); i++){
        for(j = 0; j < dimension; j++){
            patterns[i][j] = initial_pattern[j];
        }
    }

    indx = 0;
    permute(initial_pattern, 0, dimension - 1, permutations);
    indx = 0;

    for(i = 0; i < dimFat; i++){
        for(j = 0; j < dimension; j++){
            symbols[i][j] = permutations[k];
            k++;
        }
    }

    for(i = 0; i < seriesize - ((dimension - 1)*delay); i++){
        for(j = 0; j < dimension; j++){
            elements[i][j] = elements_aux[aux];
            aux++;
        }
    }

    for(i = 0; i < (seriesize - (dimension - 1)*delay); i += delay){
        double* element = elements[i];
        for(j = 1; j <= dimension; j++){
            for(k = 0; k < dimension - 1; k++){
                if(element[k] > element[k+1]){
                    
                    double aux_element = element[k];
                    int aux_pattern = patterns[i][k];

                    element[k] = element[k+1];
                    patterns[i][k] = patterns[i][k+1];
                    
                    element[k+1] = aux_element;
                    patterns[i][k+1] = aux_pattern;
                }
            }
        }
    }

    
    //Probability Distribution
    PROTECT(Rprobability = allocVector(REALSXP, dimFat));
    for(i = 0; i < dimFat; i++){
        REAL(Rprobability)[i] = 0;
    }

    for(i = 0; i < seriesize - ((dimension - 1)*delay); i++){
        for(j = 0; j < dimFat; j++){
            if(equals(patterns[i], symbols[j], dimension)){
                REAL(Rprobability)[j]++;
            }
        }
    }

    for(i = 0; i < dimFat; i++){
        REAL(Rprobability)[i] = REAL(Rprobability)[i]/(seriesize - (dimension - 1)*delay);
    }

    UNPROTECT(1);

    return Rprobability;

}

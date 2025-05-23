# Numerical Integration Methods - Pseudocode

## Setup (Common to all methods)
```
FUNCTION setup_integration(a, b, n):
    delta_x = (b - a) / n
    x = ARRAY of size (n + 1)
    
    FOR i = 0 TO n:
        x[i] = a + i * delta_x
    
    RETURN delta_x, x
```

## Left Hand Rule
```
FUNCTION left_hand_rule(f, a, b, n):
    delta_x, x = setup_integration(a, b, n)
    
    sum = 0
    FOR i = 0 TO (n - 1):
        sum = sum + f(x[i])
    
    L_n = delta_x * sum
    RETURN L_n
```

## Right Hand Rule
```
FUNCTION right_hand_rule(f, a, b, n):
    delta_x, x = setup_integration(a, b, n)
    
    sum = 0
    FOR i = 1 TO n:
        sum = sum + f(x[i])
    
    R_n = delta_x * sum
    RETURN R_n
```

## Trapezoidal Rule
```
FUNCTION trapezoidal_rule(f, a, b, n):
    delta_x, x = setup_integration(a, b, n)
    
    // First and last terms
    sum = f(x[0]) + f(x[n])
    
    // Middle terms (multiplied by 2)
    FOR i = 1 TO (n - 1):
        sum = sum + 2 * f(x[i])
    
    T_n = (delta_x / 2) * sum
    RETURN T_n
```

## Simpson's Rule
```
FUNCTION simpsons_rule(f, a, b, n):
    // Prerequisite: n must be even
    IF n MOD 2 ≠ 0:
        ERROR "n must be even for Simpson's Rule"
    
    delta_x, x = setup_integration(a, b, n)
    
    // Start with first and last terms (coefficient = 1)
    sum = f(x[0]) + f(x[n])
    
    // Odd indices (coefficient = 4)
    FOR i = 1 TO (n - 1) STEP 2:
        sum = sum + 4 * f(x[i])
    
    // Even indices (coefficient = 2)
    FOR i = 2 TO (n - 2) STEP 2:
        sum = sum + 2 * f(x[i])
    
    S_n = (delta_x / 3) * sum
    RETURN S_n
```

## Alternative Simpson's Rule (Pattern-based)
```
FUNCTION simpsons_rule_pattern(f, a, b, n):
    // Prerequisite: n must be even
    IF n MOD 2 ≠ 0:
        ERROR "n must be even for Simpson's Rule"
    
    delta_x, x = setup_integration(a, b, n)
    
    sum = 0
    FOR i = 0 TO n:
        IF i = 0 OR i = n:
            coefficient = 1          // First and last
        ELSE IF i MOD 2 = 1:
            coefficient = 4          // Odd indices
        ELSE:
            coefficient = 2          // Even indices (middle)
        
        sum = sum + coefficient * f(x[i])
    
    S_n = (delta_x / 3) * sum
    RETURN S_n
```

## Usage Example
```
FUNCTION example_usage():
    // Define function f(x) = sin(2x) + x/3
    FUNCTION f(x):
        RETURN sin(2*x) + x/3
    
    a = -1
    b = 3
    n = 100
    
    L = left_hand_rule(f, a, b, n)
    R = right_hand_rule(f, a, b, n)
    T = trapezoidal_rule(f, a, b, n)
    S = simpsons_rule(f, a, b, n)
    
    PRINT "Left Hand Rule:", L
    PRINT "Right Hand Rule:", R
    PRINT "Trapezoidal Rule:", T
    PRINT "Simpson's Rule:", S
```

## Key Notes
- **Simpson's Rule Pattern**: Coefficients are 1, 4, 2, 4, 2, 4, ..., 4, 1
- **Simpson's Rule Requirement**: n must be even
- **Accuracy**: Generally Simpson's > Trapezoidal > Left/Right Hand Rules
- **All methods**: Accuracy improves as n increases

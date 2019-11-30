/*****************************************************
* File Name: buffer.c
* Compiler: MS Visual Studio 2019
* Author: Patrick Bobyn
* Course: CST 8152 - Compilers 012
* Assignment: 1
* Date: 29 September 2019
* Professor: Sv. Ranev
* Purpose: Creates method definitions for a buffer.
* Function List: b_allocate(), b_addc(), b_clear(), b_free(), b_isfull(), b_limit(), b_capacity(), b_mark(), b_mode(), 
*				 b_incfactor(), b_load(), b_isempty(), b_getc(), b_eob(), b_print(), b_compact(), b_rflag(), b_retract(),
*				 b_reset(), b_getcoffset(), b_rewind(), b_location()
******************************************************/

#include "buffer.h"

/*****************************************************
* Purpose: The purpose of this is to allocate the memory to the buffer. Returns NULL if it does not work.
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions: calloc(), malloc(), free()
* Parameters: - init_capacity, short, has to be between 0 and SHRT_MAX
*			  - inc_factor, char, has to be unsigned and between 0 and 255
*			  - o_mode, char, has to be a, m, or f
* Return Value: Buffer*
* Algorithm: - Make sure all inputs are valid
			 - Returns NULL if:
				- init _capacity is < 0 or > SHRT_MAX
				- o_mode != 'a', 'm', or 'f'
				- inc_factor is < 0 or > 255
				- not allocated correctly
			- sets values of all the buffer settings 
*****************************************************/

Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {	
	
	/* create Buffer */
	Buffer* buffer; /* the new buffer to be returned */

	/* ERRORS: returns NULL
	init_capacity out of range ( x < 0 and x > MAX_VALUE) */
	if (init_capacity < 0)
		return NULL;

	if (init_capacity > SHRT_MAX-1)
		return NULL;

	/* o_mode has to be a, m, or f */
	if (o_mode != 'a' && o_mode != 'm' && o_mode != 'f')
		return NULL;

	/* inc_factor has to be within 1 >= x <= 255 */
	if ( (unsigned char) inc_factor < 0 || (unsigned char) inc_factor > 255)
		return NULL;
	
	/* Create the buffer and return NULL if it fails */
	if ((buffer = calloc(1, sizeof(Buffer))) == NULL)
		return NULL;

	/* if init_capacity is 0, disregard all input params except mode*/
	if (init_capacity == 0) {
		buffer->cb_head = (char*)malloc(DEFAULT_INIT_CAPACITY);

		/* create and make sure the buffer character block is valid */
		if (buffer->cb_head == NULL) {
			free(buffer->cb_head);
			return NULL;
		}

		/* switch case to assign params based off of mode */
		switch (o_mode) {
		case 'a': 
			buffer->inc_factor = 15;
			buffer->mode = 1;
			break;
		case 'm':
			buffer->inc_factor = 15;
			buffer->mode = -1;
			break;
		case 'f':
			buffer->inc_factor = 0;
			buffer->mode = 0;
			break;
		default:
			return NULL;
		}

		/* for all cases assign the capacity to the default level */
		buffer->capacity = DEFAULT_INIT_CAPACITY;

	}
	else { /* if init_capacity is not 0, assign it to another value */
		buffer->cb_head = (char*)malloc(init_capacity * sizeof(char));

		/* make sure the buffer character block is valid */
		if (buffer->cb_head == NULL) {
			free(buffer->cb_head);
			return NULL;
		}

		/* check cases based off of input params */
		if (o_mode == 'f') {
			buffer->mode = 0;
			buffer->inc_factor = 0;
		} /* inc_factor == 0 and init_capacity != 0 */
		else if ((unsigned char)inc_factor == 0 && init_capacity != 0) {
			buffer->mode = 0;
			buffer->inc_factor = 0;
		} /* mode is a, inc_factor >= 1 and inc_factor <= 255 */
		else if (o_mode == 'a' && (unsigned char)inc_factor >= 1 && (unsigned char)inc_factor <= 255) {
			buffer->mode = 1;
			buffer->inc_factor = (unsigned char)inc_factor;
		} /* mode is m, inc_factor >= 1 and inc_factor <= 100*/
		else if (o_mode == 'm' && (unsigned char)inc_factor >= 1 && (unsigned char)inc_factor <= 100) {
			buffer->mode = -1;
			buffer->inc_factor = (unsigned char)inc_factor;
		} /* return NULL if it doesnt work */
		else
			return NULL;

		/* set capacity to the param capacity */
		buffer->capacity = init_capacity;
	}

	/* set flags and return the buffer */
	buffer->flags = DEFAULT_FLAGS;
	return buffer;
}

/*****************************************************
* Purpose: The purpose of this is to add characters to the buffer. If the capacity is too small then it adds more to the size
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions: b_isfull(), realloc()
* Parameters: - pBD, pBuffer, pointer to the Buffer created
			  - symbol, char, the symbol to be added to the buffer
* Return Value: pBuffer
* Algorithm: - check to make sure buffer and the character buffer are not null
			 - add the character into the character buffer if its not full
			 - if the character buffer is full add more to the capacity and then add the symbol
*****************************************************/

pBuffer b_addc(pBuffer const pBD, char symbol) { 

	short new_capacity = 0, available, increment;
	/* new_capacity is the capacity that the buffer will be changed to if needed
	available is how much memory is available for the buffer
	increment is how much the buffer will be incremented by */
	char* temp = NULL; /* this is the temp char pointer for if the buffer needs to increased in size */

	/* make sure no runtime errors */
	if (pBD == NULL)
		return NULL;

	/* make sure character buffer is not NULL */
	if (pBD->cb_head == NULL)
		return NULL;

	/* if its not full then add character */
	if (b_isfull(pBD) == 0) {
		pBD->flags &= RESET_R_FLAG;
		*(pBD->cb_head + pBD->addc_offset) = symbol;
		pBD->markc_offset++;
		pBD->addc_offset++;
		return pBD;
	}

	/* if its full then resize with a larger capacity base mode */
	if (b_isfull(pBD) != 0) {

		/* if mode is 0 return NULL */
		if (pBD->mode == 0)
			return NULL;
		else if (pBD->mode == 1) {		/* if the mode is 1 resize */

			new_capacity = pBD->capacity + (unsigned char)pBD->inc_factor;
			if (new_capacity > SHRT_MAX)  /* Assign maximum limit to new value */
				new_capacity = SHRT_MAX - 1;

			if (new_capacity < 0) /* If its negative then return NULL */
				return NULL;
		}
		else if (pBD->mode ==  -1) {		/* if the mode is -1 resize */
			if (pBD->capacity == SHRT_MAX - 1)
				return NULL;

			/* get values */
			available = SHRT_MAX - 1 - pBD->capacity;
			increment = (available * (unsigned char)pBD->inc_factor) / 100;
			new_capacity = pBD->capacity + increment;

			/* check values */
			if (increment == 0) {
				if (new_capacity < SHRT_MAX-1)
						new_capacity = SHRT_MAX-1;
				else
					return NULL;
			} 
		}
	}

	pBD->capacity = new_capacity; /* set new capacity */

	/* return NULL if it cant be reallocated */
	if ((temp = (char*) realloc( pBD->cb_head, pBD->capacity)) == NULL)
		return NULL;

	/* add the temp value back to cb_head and change buffer settings */
	pBD->cb_head = temp;
	pBD->flags |= SET_R_FLAG;
	if (pBD->cb_head && pBD->addc_offset)
		*(pBD->cb_head + pBD->addc_offset) = symbol;
	pBD->markc_offset++;
	pBD->addc_offset++;
	return pBD;
}

/*****************************************************
* Purpose: The purpose of this is to reset the entire buffer settings back to 0
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions: 
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: int
* Algorithm: 
*****************************************************/

int b_clear(Buffer* const pBD) { 
	
	/* return NULL if pBD is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* set all the settings back to their default params */
	pBD->addc_offset = 0;
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	pBD->flags = DEFAULT_FLAGS;

	return PASS;
}

/*****************************************************
* Purpose: The purpose of this is to free the buffer
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions: free()
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: void
* Algorithm: 
*****************************************************/

void b_free(Buffer* const pBD) { 

	/* return NULL if the buffer is already NULL */
	if (pBD == NULL) 
		return;
	
	/* free the buffer */
	free(pBD);
}

/*****************************************************
* Purpose: The purpose of this is see if the buffer is full
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions: 
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: int
* Algorithm: 
*****************************************************/

int b_isfull(Buffer* const pBD) { 
	
	/* check if the buffer is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* return 1 if the buffer is full */

	if (pBD->capacity == pBD->markc_offset)
		return FAIL;

	/* return 0 if the buffer is not full */
	return PASS;
}

/*****************************************************
* Purpose: The purpose of this is to return where the buffer ends
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: short
* Algorithm: 
*****************************************************/

short b_limit(Buffer* const pBD) {

	/* return NULL if the buffer is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* return the addc_offset */
	return pBD->addc_offset;
}

/*****************************************************
* Purpose: The purpose of this is to return the capacity of the buffer
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: short
* Algorithm: 
*****************************************************/

short b_capacity(Buffer* const pBD) {
	
	/* returns -1 if the buffer is NULL */
	if (pBD == NULL) 
		return RT_FAIL_1;

	/* returns the capacity of the buffer */
	return pBD->capacity;
}

/*****************************************************
* Purpose: The purpose of this is to set the markc_offset param
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
*			  - mark, short, the value that markc_offset will be set to
* Return Value: short
* Algorithm:
*****************************************************/

short b_mark(pBuffer const pBD, short mark) {

	/* return 1 if the buffer is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* set markc_offset to mark if its within a valid range */
	if (mark >= 0 && mark <= pBD->addc_offset)
		pBD->markc_offset = mark;
	else
		return RT_FAIL_1;

	return pBD->markc_offset;
}

/*****************************************************
* Purpose: The purpose of this is to return the mode
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: int
* Algorithm:
*****************************************************/

int b_mode(Buffer* const pBD) {

	/* return 1 if the buffer is empty */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* return the mode */
	return pBD->mode;
}

/*****************************************************
* Purpose: The purpose of this is to return the inc_factor 
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: size_t
* Algorithm:
*****************************************************/

size_t b_incfactor(Buffer* const pBD) {

	/* return the DEFAULT LOCATION if the buffer is NULL */
	if (pBD == NULL)
		return DEFAULT_LOCATION;

	/* return inc_factor */
	return (unsigned char) pBD->inc_factor;
}

/*****************************************************
* Purpose: The purpose of this is to load a file into the buffer
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions: feof(), fgetc(), b_addc(), ungetc()
* Parameters: - fi, FILE*, a pointer to the file that is open
*		      - pBD, pBuffer, pointer to the Buffer created
* Return Value: int
* Algorithm: - Return RT_FAIL_1 if the buffer or the file are NULL
			 - while the character is not EOF then add the character to the buffer
*****************************************************/

int b_load(FILE* const fi, Buffer* const pBD) {

	char c; /* the character being read from the file */
	int num = 0; /* the number of characters in the file */

	/* return RT_FAIL_1 if the buffer or File are NULL */
	if (pBD == NULL || fi == NULL)
		return RT_FAIL_1;

	/* while the file is not EOF get the character and add it to the buffer */
	while (!feof(fi)) {
		c = (char)fgetc(fi);
		if (c != EOF) {
			if (b_addc(pBD, c) == NULL) {
				ungetc(c, fi); /* if the character cant be added then put it back into the file */
				return LOAD_FAIL;
			}
			num++; /* increment the counter */
		}
	}

	/* return the counter */
	return num;
}

/*****************************************************
* Purpose: The purpose of this is to make sure the buffer isn't empty
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: int
* Algorithm:
*****************************************************/

int b_isempty(Buffer* const pBD) {

	/* check to make sure the buffer isn't NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* return FAIL if the buffer is empty */
	if (pBD->addc_offset == 0)
		return FAIL;

	/* return pass otherwise */
	return PASS;
}

/*****************************************************
* Purpose: The purpose of this is to get the character from the buffer
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: char
* Algorithm:
*****************************************************/

char b_getc(Buffer* const pBD) {
	
	/* return RT_FAIL_2 if the buffer is NULL or if the charcter buffer is NULL */
	if (pBD == NULL) 
		return RT_FAIL_2;

	if (pBD->cb_head == NULL)
		return RT_FAIL_2;

	/* if the file is at the end of the buffer then set the flags and return PASS */
	if (pBD->getc_offset == pBD->addc_offset) {
		pBD->flags |= SET_EOB;
		return PASS;
	}
	else { /* else make sure the flags are not set */
		pBD->flags &= RESET_EOB;
	}

	/* return the next character in the buffer */
	return (char) * (pBD->cb_head + pBD->getc_offset++);
}

/*****************************************************
* Purpose: The purpose of this is to check if the end of the buffer has been hit
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: int
* Algorithm:
*****************************************************/

int b_eob(Buffer* const pBD) {

	/* check to make sure the buffer isn't NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* return the flags value with the CHECK_EOB definition */
	return pBD->flags & CHECK_EOB;
}

/*****************************************************
* Purpose: The purpose of this is to print out the contents of the buffer
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions: b_isempty(), b_getc(), b_eob()
* Parameters: - pBD, pBuffer, pointer to the Buffer created
*			  - nl, char, the New Line character 
* Return Value: short
* Algorithm:
*****************************************************/

int b_print(Buffer* const pBD, char nl) {

	char c; /* used to store the character from the buffer */
	int i = 0, num = 0; /* i is used for a for loop, num is the number of characters printed off */

	/* Diagnostic Tool 
	return RT_FAIL_1 if the buffer is NU:: */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* if the buffer is not empty then go through it to print off the next character */
	if (!b_isempty(pBD)) {
		for (i = 0; i <= pBD->addc_offset; i++) { /* for every char in the buffer */
			c = b_getc(pBD); /* get the character and save it */
			if (c == RT_FAIL_2)
				return RT_FAIL_2;
			else if (b_eob(pBD)) /* make sure the char is not the end of the buffer */
				continue;
			else {	/* print the character */
				num++;
				printf("%c", c);
			}
		}
	}

	if (nl != 0)	/* print the newline character */
		printf("\n");

	/* return the number of characters printed */
	return num;
}

/*****************************************************
* Purpose: The purpose of this is to add one more character to the end of the buffer
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
*			  - symbol, char, the character to be added to the end of the buffer
* Return Value: Buffer*
* Algorithm:
*****************************************************/

Buffer* b_compact(Buffer* const pBD, char symbol) {

	unsigned short new_capacity; /* the new capacity for the buffer */
	char* new_head = NULL; /* a temp pointer for the character buffer */

	/* if the buffer is NULL return NULL */
	if (pBD == NULL)
		return NULL;

	/* set the new capacity to the end of the current buffer plus 1 character */
	new_capacity = pBD->addc_offset + 1;

	/* if the new capacity is negative return NULL */
	if (new_capacity < 0)
		return NULL;
	
	/* try to allocate the new buffer using the temp value, return NULL if it doesnt work */
	if ((new_head = (char*)realloc(pBD->cb_head, new_capacity)) == NULL)
		return NULL;

	/* add the new character to the newest location */
	pBD->cb_head = new_head;
	*(pBD->cb_head + pBD->addc_offset) = symbol;
	pBD->addc_offset++;
	pBD->capacity = new_capacity;
	pBD->flags |= SET_R_FLAG;

	/* return the buffer */
	return pBD;
}

/*****************************************************
* Purpose: The purpose of this is return the value of the rflag bit
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: char 
* Algorithm:
*****************************************************/

char b_rflag(Buffer* const pBD) {

	/* return RT_FAIL_1 if the buffer is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* return the rflag bit */
	return pBD->flags & CHECK_R_FLAG;
}

/*****************************************************
* Purpose: The purpose of this is to retract the getc_offset by one character
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions: b_getcoffset()
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: short
* Algorithm:
*****************************************************/

short b_retract(Buffer* const pBD) {

	/* return NULL if the buffer is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* return RT_FAIL_1 if the getcoffset is 0 */
	if (b_getcoffset(pBD) == 0)
		return RT_FAIL_1;

	/* return the getc_offset -1 */
	return --pBD->getc_offset;
}

/*****************************************************
* Purpose: The purpose of this is to set the getc_offset to the markc_offset
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: short
* Algorithm:
*****************************************************/

short b_reset(Buffer* const pBD) {

	/* return RT_FAIL_1 if the buffer is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* set the getc_offset to markc_offset and return the new getc_offset */
	pBD->getc_offset = pBD->markc_offset;
	return pBD->getc_offset;
}

/*****************************************************
* Purpose: The purpose of this is to return the getc_offset
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: short
* Algorithm:
*****************************************************/

short b_getcoffset(Buffer* const pBD) {

	/* return RT_FAIL_1 if the buffer is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* return the getc_offset */
	return pBD->getc_offset;
}

/*****************************************************
* Purpose: The purpose of this is to reset the buffer to the initial values after it has been allocated 
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: int 
* Algorithm:
*****************************************************/

int b_rewind(Buffer* const pBD) {

	/* return RT_FAIL_1 if the buffer is NULL */
	if (pBD == NULL)
		return RT_FAIL_1;

	/* reset the values to the original values */
	pBD->getc_offset = 0;
	pBD->markc_offset = 0;
	return PASS;
}

/*****************************************************
* Purpose: The purpose of this is to return the location of the markc_offset
* Author: Patrick Bobyn
* History/Version: 29 September 2019	1.0
* Called Functions:
* Parameters: - pBD, pBuffer, pointer to the Buffer created
* Return Value: char* 
* Algorithm:
*****************************************************/

char* b_location(Buffer* const pBD) {

	/* return NULL if the buffer is NULL */
	if (pBD == NULL)
		return NULL;

	/* return the location of the markc_offset */
	return (char*) &pBD->markc_offset;
}
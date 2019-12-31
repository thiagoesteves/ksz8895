/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    ksz8895_driver.h
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains functions to read and write messages from
 *          and to the ksz8895 gen_server
 */

#ifndef KSZ8895_C_SRC_KSZ8895_DRIVER_H
#define KSZ8895_C_SRC_KSZ8895_DRIVER_H

/**
 * @brief Error defines
 */

#define KSZ8895_OK    (0)
#define KSZ8895_ERROR (-1)

/** @brief This function opens the driver that will handle the ksz8895 transactions
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int open_ksz8895_driver(char *buf, int *index);

/** @brief This function closes the driver that will handle the ksz8895 transactions
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int close_ksz8895_driver(char *buf, int *index);

/** @brief Read KSZ8895 register, this function expects 2 arguments from KSZ8895 
 *         driver in Erlang:
 *         @param Instance KSZ8895 instance
 *         @param Register Register to read
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int read_register(char *buf, int *index);

/** @brief Write at KSZ8895 register, this function expects 3 arguments from KSZ8895 
 *         driver in Erlang:
 *         @param Instance KSZ8895 instance
 *         @param Register Register to write
 *         @param Value Value to write
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int write_register(char *buf, int *index);

/** @brief Read KSZ8895 pin, this function expects 2 arguments from KSZ8895 
 *         driver in Erlang:
 *         @param Instance KSZ8895 instance
 *         @param Pin Pin to read
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int read_pin(char *buf, int *index);

/** @brief Write at KSZ8895 pin, this function expects 3 arguments from KSZ8895 
 *         driver in Erlang:
 *         @param Instance KSZ8895 instance
 *         @param Pin Pin to write
 *         @param Value Value to write
 *
 * @return  < 0 on error
 *         == 0 on success
 */
int write_pin(char *buf, int *index);

#endif /* KSZ8895_C_SRC_KSZ8895_DRIVER_H */
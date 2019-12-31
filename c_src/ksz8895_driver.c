
/****************************************************************************
 * Copyright (C) 2020 by Thiago Esteves.                                    *
 ****************************************************************************/

/**
 * @file    ksz8895_driver.c
 * @author  Thiago Esteves
 * @date    03 Jan 2020
 * @brief   This file contains functions to read/write data from KSZ8895
 */

#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "erl_comm.h"
#include "ksz8895_driver.h"

/**
 * @brief The nex information will be a struct with all 
 *        data information to emulate a physical device.
 *        This information is not needed in a real device
 */
#define KSZ8895_MAX_INSTANCES (20)
#define KSZ8895_MAX_REGISTERS (256)

typedef enum
{
  KSZ8895_PIN_RESET = 0,
  KSZ8895_MAX_PIN
} ksz8895_pin_e;

typedef struct
{
  uint8_t pin[KSZ8895_MAX_PIN];
  uint8_t data[KSZ8895_MAX_REGISTERS];
} stub_ksz8895_info_t;

static stub_ksz8895_info_t stub_ksz8895_info[KSZ8895_MAX_INSTANCES];

const uint8_t stub_ksz8895_default_data[KSZ8895_MAX_REGISTERS] = 
{
/*  00: */ 0x95,0xAE,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/*  16: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,0x00,0x00,0x00,0x3F,0x00,
/*  32: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x10,0x02,0x00,0x00,0x00,0x1F,0x01,
/*  48: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,0x00,0x00,0x00,0x3F,0x00,
/*  64: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x10,0x02,0x00,0x00,0x00,0x1F,0x01,
/*  80: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1E,0x00,0x00,0x00,0x00,0x3F,0x00,
/*  96: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 112: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 128: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 144: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 160: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 176: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 192: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 208: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 224: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
/* 240: */ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
};


int open_ksz8895_driver(char *buf, int *index)
{
  /*TODO: Insert the opening of the driver here */

  /* STUB CODE: Initilise Emulator data for all KSZ8895's */
  for (int i = 0; i < KSZ8895_MAX_INSTANCES; i++) {
    for (int j = 0; j < KSZ8895_MAX_PIN; j++) {
      stub_ksz8895_info[i].pin[j] = 0x00;
    }
    memcpy(&stub_ksz8895_info[i].data[0], &stub_ksz8895_default_data[0], KSZ8895_MAX_REGISTERS);
  }
  return send_answer_string_ulong("ok", KSZ8895_OK);
}

int close_ksz8895_driver(char *buf, int *index)
{
  /*TODO: Insert the closing of the driver here */
  
  return send_answer_string_ulong("ok", KSZ8895_OK);
}

int read_register(char *buf, int *index)
{
  unsigned long instance, reg;
  uint8_t read_value;
  
  if (ei_decode_ulong(buf, index, &instance) ||
      ei_decode_ulong(buf, index, &reg))
  {
      return KSZ8895_ERROR;
  }

  /*TODO: Insert the reading of the ksz8895 register here */

  /* STUB CODE: Initilise Emulator data for all KSZ8895's */
  static uint8_t value = 0;
  if (reg == 120) {
    read_value = ++value;
  } else if (reg == 117) {
    read_value = 0x40;
  } else {
    read_value = stub_ksz8895_info[instance].data[reg];
  }

  return send_answer_string_ulong("ok", (uint32_t)read_value);
}

int write_register(char *buf, int *index)
{
  unsigned long instance, reg, value;
  
  if (ei_decode_ulong(buf, index, &instance) ||
      ei_decode_ulong(buf, index, &reg)      ||
      ei_decode_ulong(buf, index, &value))
  {
      return KSZ8895_ERROR;
  }

  /*TODO: Insert the reading of the ksz8895 pin here */

  /* STUB CODE: Initilise Emulator data for all KSZ8895's */
  stub_ksz8895_info[instance].data[reg] = (uint8_t)value;

  return send_answer_string_ulong("ok", KSZ8895_OK);
}

int read_pin(char *buf, int *index)
{
  unsigned long instance, pin;
  uint8_t pin_state;
  
  if (ei_decode_ulong(buf, index, &instance) ||
      ei_decode_ulong(buf, index, &pin))
  {
      return KSZ8895_ERROR;
  }

  /*TODO: Insert the reading of the ksz8895 pin here */

  /* STUB CODE: Initilise Emulator data for all KSZ8895's */
  pin_state = (unsigned long)stub_ksz8895_info[instance].pin[pin];

  return send_answer_string_ulong("ok", (uint32_t)pin_state);
}

int write_pin(char *buf, int *index)
{
  unsigned long instance, pin, value;
  
  if (ei_decode_ulong(buf, index, &instance) ||
      ei_decode_ulong(buf, index, &pin)      ||
      ei_decode_ulong(buf, index, &value))
  {
      return KSZ8895_ERROR;
  }

  /*TODO: Insert the reading of the ksz8895 pin here */

  /* STUB CODE: Initilise Emulator data for all KSZ8895's */
  stub_ksz8895_info[instance].pin[pin] = (uint8_t)value;

  return send_answer_string_ulong("ok", KSZ8895_OK);
}
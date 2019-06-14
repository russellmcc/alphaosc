#!/bin/bash

# burn fuses
avrdude -c avrispmkii -p m328p -U lfuse:w:0xB7:m

# program chip
avrdude -c avrispmkii -p m328p -U flash:w:arduino.ino.standard.hex
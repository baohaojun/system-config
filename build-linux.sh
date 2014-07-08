#!/bin/bash
qtchooser -qt=5 -run-tool=qmake && make -j8 && ./T1Wrench

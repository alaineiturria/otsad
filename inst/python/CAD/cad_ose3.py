# -*- coding: utf-8 -*-
# -----------------------------------------------------------------------------
# Contextual Anomaly Detector - Open Source Edition
# Copyright (C) 2016, Mikhail Smirnov   smirmik@gmail.com
# https://github.com/smirmik/CAD
#
# This program is free software: you can redistribute it and/or modify it under
# the terms  of  the  GNU Affero Public License version 3  as  published by the
# Free Software Foundation.
#
# This program is distributed  in  the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Affero Public License for more details.
#
# You should have received a copy of the  GNU Affero Public License  along with
# this program.  If not, see http://www.gnu.org/licenses.
#
# ------------------------------------------------------------------------------

# from contextOperator import ContextOperator
from context_operator3 import ContextOperator

class ContextualAnomalyDetectorOSE(object):
    '''
    Contextual Anomaly Detector - Open Source Edition
    https://github.com/smirmik/CAD
    '''

    def __init__(self,  minValue,
                        maxValue,
                        baseThreshold = 0.75,
                        restPeriod = 30,
                        maxLeftSemiContextsLenght = 7,
                        maxActiveNeuronsNum = 15,
                        numNormValueBits = 3,
                        lib = 1) :

        self.minValue = float(minValue)
        self.maxValue = float(maxValue)
        self.restPeriod = int(restPeriod)
        self.baseThreshold = float(baseThreshold)
        self.maxActiveNeuronsNum = int(maxActiveNeuronsNum)
        self.numNormValueBits = int(numNormValueBits)

        self.maxBinValue = 2 ** self.numNormValueBits - 1.0
        self.fullValueRange = self.maxValue - self.minValue
        if self.fullValueRange == 0.0 :
            self.fullValueRange = self.maxBinValue
        self.minValueStep = self.fullValueRange / self.maxBinValue

        self.leftFactsGroup = tuple()

        self.contextOperator = ContextOperator( maxLeftSemiContextsLenght, lib )

        self.potentialNewContexts = []

        self.lastPredictionedFacts = []
        self.resultValuesHistory = [ 1.0 ]


    def step(self, inpFacts):

        currSensFacts = tuple(sorted(set(inpFacts)))

        if len(self.leftFactsGroup) > 0 and len(currSensFacts) > 0 :
            potNewZeroLevelContext = tuple([self.leftFactsGroup,currSensFacts])
            newContextFlag = self.contextOperator.getContextByFacts([potNewZeroLevelContext], zerolevel = 1)
        else :
            potNewZeroLevelContext = False
            newContextFlag = False

        activeContexts, numSelectedContext, potentialNewContextList =  self.contextOperator.contextCrosser (
                                                                            leftOrRight = 1,
                                                                            factsList = currSensFacts,
                                                                            newContextFlag = newContextFlag
                                                                        )

        numUniqPotNewContext = len(set(potentialNewContextList).union([potNewZeroLevelContext]) if potNewZeroLevelContext else set(potentialNewContextList))

        percentSelectedContextActive = len(activeContexts) / float(numSelectedContext) if numSelectedContext > 0 else 0.0

        activeContexts = sorted(activeContexts, key=lambda con: (con[1], con[2], con[3]))
        activeNeurons = [ activeContextInfo[0] for activeContextInfo in activeContexts[-self.maxActiveNeuronsNum:] ]

        currNeurFacts = set([ 2 ** 31 + fact for fact in activeNeurons ])

        self.leftFactsGroup = set()
        self.leftFactsGroup.update(currSensFacts, currNeurFacts)
        self.leftFactsGroup = tuple(sorted(self.leftFactsGroup))

        numNewContexts  =  self.contextOperator.contextCrosser  (
                                                        leftOrRight = 0,
                                                        factsList = self.leftFactsGroup,
                                                        potentialNewContexts = potentialNewContextList
                                                    )

        numNewContexts += 1 if newContextFlag else 0

        percentAddedContextToUniqPotNew = numNewContexts / float(numUniqPotNewContext) if newContextFlag and numUniqPotNewContext > 0 else 0.0

        return percentSelectedContextActive, percentAddedContextToUniqPotNew


    def getAnomalyScore(self,inputData):

        normInputValue = int((inputData - self.minValue) / self.minValueStep)
        binInputNormValue = bin(normInputValue).lstrip("0b").rjust(self.numNormValueBits,"0")

        outSens = set([ sNum * 2 + ( 1 if currSymb == "1" else 0 ) for sNum, currSymb in enumerate(reversed(binInputNormValue)) ])

        anomalyValue0, anomalyValue1 = self.step(outSens)

        currentAnomalyScore = (1.0 - anomalyValue0 + anomalyValue1) / 2.0

        returnedAnomalyScore = currentAnomalyScore if max(self.resultValuesHistory[-int(self.restPeriod):]) < self.baseThreshold else 0.0
        self.resultValuesHistory.append(currentAnomalyScore)

        return returnedAnomalyScore



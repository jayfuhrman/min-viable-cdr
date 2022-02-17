#ifndef _BISECT_ALL_H_
#define _BISECT_ALL_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
* \file bisect_all.h
* \ingroup Objects
* \brief This is the header file for the BisectAll solver component class.
*
* \author Josh Lurz
*/
#include <string>

#include "solution/solvers/include/solver_component.h"

class CalcCounter; 
class Marketplace;
class World;
class SolutionInfoSet;
class ISolutionInfoFilter;

/*! 
* \ingroup Objects
* \brief A class interface which defines the BisectAll solver component.
* \author Josh Lurz
*/

class BisectAll: public SolverComponent {
public:        
    BisectAll( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn );
    BisectAll();
    virtual ~BisectAll();
    static const std::string& getXMLNameStatic();
    
    // SolverComponent methods
    virtual void init();
    virtual ReturnCode solve( SolutionInfoSet& aSolutionSet, const int aPeriod );
    virtual const std::string& getXMLName() const;

    // AParsable methods
    virtual bool XMLParse( rapidxml::xml_node<char>* & aNode );

protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        SolverComponent,
        
        //! Max iterations for this solver component
        DEFINE_VARIABLE( SIMPLE, "max-iterations", mMaxIterations, unsigned int ),
        
        //! Default bracket interval to use for bracketing, could be overridden by a SolutionInfo
        DEFINE_VARIABLE( SIMPLE, "bracket-interval", mDefaultBracketInterval, double ),
                            
        //! Bracket tolerance for checking left-right bracket convergence
        DEFINE_VARIABLE( SIMPLE, "bracket-tolerance", mBracketTolerance, double ),
        
        //! Max iterations for bracketing
        DEFINE_VARIABLE( SIMPLE, "max-bracket-iterations", mMaxBracketIterations, unsigned int ),
                            
        //! Flag to configure if we should use the secant method to find brackets (default false)
        DEFINE_VARIABLE( SIMPLE, "use-secant-brackets", mUseSecantBrackets, bool ),
        
        //! A filter which will be used to determine which SolutionInfos this solver component
        //! will work on.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "solution-info-filter", mSolutionInfoFilter, ISolutionInfoFilter*  )
    )
    
    bool areAllBracketsEqual( SolutionInfoSet& aSolutionSet ) const;
};

#endif // _BISECT_ALL_H_

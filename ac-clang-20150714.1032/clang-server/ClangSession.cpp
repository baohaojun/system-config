/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2015/06/26.18:05:06 */

/*
 * Copyright (c) 2013-2015 yaruopooner [https://github.com/yaruopooner]
 *
 * This file is part of ac-clang.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


/*================================================================================================*/
/*  Comment                                                                                       */
/*================================================================================================*/


/*================================================================================================*/
/*  Include Files                                                                                 */
/*================================================================================================*/

#include <regex>

#include "ClangSession.hpp"


using   namespace   std;



/*================================================================================================*/
/*  Internal Printer Class Definitions Section                                                    */
/*================================================================================================*/



class   ClangSession::Completion
{
public:
    Completion( ClangSession& Session ) :
        m_Session( Session )
    {
    }
        
    void    PrintCompleteCandidates( void );

private:
    bool    PrintCompletionHeadTerm( CXCompletionString CompletionString );
    void    PrintAllCompletionTerms( CXCompletionString CompletionString );
    void    PrintCompletionLine( CXCompletionString CompletionString );
    void    PrintCompletionResults( CXCodeCompleteResults* CompleteResults );

private:
    ClangSession&       m_Session;
};


class   ClangSession::Diagnostics
{
public:
    Diagnostics( ClangSession& Session ) :
        m_Session( Session )
    {
    }

    void    PrintDiagnosticsResult( void );

private:
    ClangSession&       m_Session;
};


class   ClangSession::Jump
{
public:
    Jump( ClangSession& Session ) :
        m_Session( Session )
    {
    }
        
    void    PrintDeclarationLocation( void );
    void    PrintDefinitionLocation( void );
    void    PrintSmartJumpLocation( void );

private:
    void    PrepareTransaction( uint32_t& Line, uint32_t& Column );
    bool    PrintExpansionLocation( CXCursor (*pCursorFunctionCallback)( CXCursor ), const uint32_t Line, const uint32_t Column );

    ClangSession&       m_Session;
};




bool    ClangSession::Completion::PrintCompletionHeadTerm( CXCompletionString CompletionString )
{
    // check accessibility of candidate. (access specifier of member : public/protected/private)
    if ( clang_getCompletionAvailability( CompletionString ) == CXAvailability_NotAccessible )
    {
        return ( false );
    }

    const uint32_t  n_chunks = clang_getNumCompletionChunks( CompletionString );

    // inspect all chunks only to find the TypedText chunk
    for ( uint32_t i_chunk = 0; i_chunk < n_chunks; ++i_chunk )
    {
        if ( clang_getCompletionChunkKind( CompletionString, i_chunk ) == CXCompletionChunk_TypedText )
        {
            // We got it, just dump it to fp
            CXString    ac_string = clang_getCompletionChunkText( CompletionString, i_chunk );

            m_Session.m_Writer.Write( "COMPLETION: %s", clang_getCString( ac_string ) );

            clang_disposeString( ac_string );

            // care package on the way
            return ( true );
        }
    }

    // We haven't found TypedText chunk in CompletionString
    return ( false );
}

void    ClangSession::Completion::PrintAllCompletionTerms( CXCompletionString CompletionString )
{
    const uint32_t  n_chunks = clang_getNumCompletionChunks( CompletionString );

    for ( uint32_t i_chunk = 0; i_chunk < n_chunks; ++i_chunk )
    {
        // get the type and completion text of this chunk
        CXCompletionChunkKind   chk_kind = clang_getCompletionChunkKind( CompletionString, i_chunk );
        CXString                chk_text = clang_getCompletionChunkText( CompletionString, i_chunk );
        
        // differenct kinds of chunks has various output formats
        switch ( chk_kind )
        {
            case    CXCompletionChunk_Placeholder:
                m_Session.m_Writer.Write( "<#%s#>", clang_getCString( chk_text ) );
                break;
                
            case    CXCompletionChunk_ResultType:
                m_Session.m_Writer.Write( "[#%s#]", clang_getCString( chk_text ) );
                break;

            case    CXCompletionChunk_Optional:
                // print optional term in a recursive way
                m_Session.m_Writer.Write( "{#" );
                PrintAllCompletionTerms( clang_getCompletionChunkCompletionString( CompletionString, i_chunk ) );
                m_Session.m_Writer.Write( "#}" );
                break;
                
            default:
                m_Session.m_Writer.Write( "%s", clang_getCString( chk_text ) );
        }

        clang_disposeString( chk_text );
    }
}

void    ClangSession::Completion::PrintCompletionLine( CXCompletionString CompletionString )
{
    // print completion item head: COMPLETION: typed_string
    if ( PrintCompletionHeadTerm( CompletionString ) )
    {
        // If there's not only one TypedText chunk in this completion string,
        //  * we still have a lot of info to dump: 
        //  *
        //  *     COMPLETION: typed_text : ##infos## \n
        m_Session.m_Writer.Write( " : " );

        PrintAllCompletionTerms( CompletionString );

        m_Session.m_Writer.Write( "\n" );
    }
}

void    ClangSession::Completion::PrintCompletionResults( CXCodeCompleteResults* CompleteResults )
{
    const uint32_t  results_limit = m_Session.m_Context.GetCompleteResultsLimit();
    const bool      is_accept     = results_limit ? ( CompleteResults->NumResults < results_limit ) : true;

    if ( !is_accept )
    {
        m_Session.m_Writer.Write( "A number of completion results(%d) is threshold value(%d) over!!\n", CompleteResults->NumResults, results_limit );
        return;
    }

    for ( uint32_t i = 0; i < CompleteResults->NumResults; ++i )
    {
        PrintCompletionLine( CompleteResults->Results[ i ].CompletionString );
    }
}

void    ClangSession::Completion::PrintCompleteCandidates( void )
{
    uint32_t        line;
    uint32_t        column;
    
    m_Session.m_Reader.ReadToken( "line:%d", line );
    m_Session.m_Reader.ReadToken( "column:%d", column );
    
    m_Session.ReadSourceCode();

    CXUnsavedFile               unsaved_file = m_Session.GetCXUnsavedFile();
    // necessary call?
    // clang_reparseTranslationUnit( m_Session.m_CxTU, 1, &unsaved_file, m_Session.m_TranslationUnitFlags );
    CXCodeCompleteResults*      results      = clang_codeCompleteAt( m_Session.m_CxTU, m_Session.m_SessionName.c_str(), line, column, &unsaved_file, 1, m_Session.m_CompleteAtFlags );

    if ( results )
    {
        clang_sortCodeCompletionResults( results->Results, results->NumResults );

        PrintCompletionResults( results );

        clang_disposeCodeCompleteResults( results );
    }

    m_Session.m_Writer.Flush();
}



void    ClangSession::Diagnostics::PrintDiagnosticsResult( void )
{
    m_Session.ReadSourceCode();
    
    CXUnsavedFile   unsaved_file = m_Session.GetCXUnsavedFile();

    clang_reparseTranslationUnit( m_Session.m_CxTU, 1, &unsaved_file, m_Session.m_TranslationUnitFlags );

    const uint32_t  n_diagnostics = clang_getNumDiagnostics( m_Session.m_CxTU );

    for ( uint32_t i = 0; i < n_diagnostics; ++i )
    {
        CXDiagnostic    diagnostic = clang_getDiagnostic( m_Session.m_CxTU, i );
        CXString        message    = clang_formatDiagnostic( diagnostic, clang_defaultDiagnosticDisplayOptions() );

        m_Session.m_Writer.Write( "%s\n", clang_getCString( message ) );

        clang_disposeString( message );
        clang_disposeDiagnostic( diagnostic );
    }

    m_Session.m_Writer.Flush();
}



void    ClangSession::Jump::PrepareTransaction( uint32_t& Line, uint32_t& Column )
{
    m_Session.m_Reader.ReadToken( "line:%d", Line );
    m_Session.m_Reader.ReadToken( "column:%d", Column );
    
    m_Session.ReadSourceCode();

    CXUnsavedFile       unsaved_file = m_Session.GetCXUnsavedFile();

    clang_reparseTranslationUnit( m_Session.m_CxTU, 1, &unsaved_file, m_Session.m_TranslationUnitFlags );
}

bool    ClangSession::Jump::PrintExpansionLocation( CXCursor (*pCursorFunctionCallback)( CXCursor ), const uint32_t Line, const uint32_t Column )
{
    CXFile              source_file     = clang_getFile( m_Session.m_CxTU, m_Session.m_SessionName.c_str() );
    CXSourceLocation    source_location = clang_getLocation( m_Session.m_CxTU, source_file, Line, Column );
    CXCursor            source_cursor   = clang_getCursor( m_Session.m_CxTU, source_location );
    
    if ( clang_isInvalid( source_cursor.kind ) )
    {
        return ( false );
    }
    
    CXCursor            dest_cursor = pCursorFunctionCallback( source_cursor );;
    
    if ( clang_isInvalid( dest_cursor.kind ) )
    {
        return ( false );
    }

    CXSourceLocation    dest_location = clang_getCursorLocation( dest_cursor );
    CXFile              dest_file;
    uint32_t            dest_line;
    uint32_t            dest_column;
    uint32_t            dest_offset;

    clang_getExpansionLocation( dest_location, &dest_file, &dest_line, &dest_column, &dest_offset );

    CXString            dest_filename  = clang_getFileName( dest_file );
    const string        path           = clang_getCString( dest_filename );
    const regex         expression( "[\\\\]+" );
    const string        replace( "/" );
    const string        normalize_path = regex_replace( path, expression, replace );
    
    clang_disposeString( dest_filename );
    m_Session.m_Writer.Write( "\"%s\" %d %d ", normalize_path.c_str(), dest_line, dest_column );

    return ( true );
}


void    ClangSession::Jump::PrintDeclarationLocation( void )
{
    uint32_t        line;
    uint32_t        column;

    PrepareTransaction( line, column );
    PrintExpansionLocation( clang_getCursorReferenced, line, column );

    m_Session.m_Writer.Flush();
}

void    ClangSession::Jump::PrintDefinitionLocation( void )
{
    uint32_t        line;
    uint32_t        column;

    PrepareTransaction( line, column );
    PrintExpansionLocation( clang_getCursorDefinition, line, column );

    m_Session.m_Writer.Flush();
}

void    ClangSession::Jump::PrintSmartJumpLocation( void )
{
    uint32_t        line;
    uint32_t        column;

    PrepareTransaction( line, column );

    if ( !PrintExpansionLocation( clang_getCursorDefinition, line, column ) )
    {
        PrintExpansionLocation( clang_getCursorReferenced, line, column );
    }

    m_Session.m_Writer.Flush();
}




/*================================================================================================*/
/*  Global Class Method Definitions Section                                                       */
/*================================================================================================*/


ClangSession::ClangSession( const std::string& SessionName, const ClangContext& Context, StreamReader& Reader, StreamWriter& Writer )
    :
    m_SessionName( SessionName )
    , m_Context( Context )
    , m_Reader( Reader )
    , m_Writer( Writer )
    , m_CxTU( nullptr )
    , m_TranslationUnitFlags( Context.GetTranslationUnitFlags() )
    , m_CompleteAtFlags( Context.GetCompleteAtFlags() )
{
}


ClangSession::~ClangSession( void )
{
    Deallocate();
}



void    ClangSession::ReadCFlags( void )
{
    int32_t             num_cflags;

    m_Reader.ReadToken( "num_cflags:%d", num_cflags );

    vector< string >    cflags;
    
    for ( int32_t i = 0; i < num_cflags; ++i )
    {
        // CFLAGS's white space must be accept.
        // a separator is '\n'.
        cflags.push_back( m_Reader.ReadToken( "%[^\n]" ) );
    }

    m_CFlagsBuffer.Allocate( cflags );
}

void    ClangSession::ReadSourceCode( void )
{
    int32_t             src_length;

    m_Reader.ReadToken( "source_length:%d", src_length );

    m_CSourceCodeBuffer.Allocate( src_length );
    
    m_Reader.Read( m_CSourceCodeBuffer.GetBuffer(), m_CSourceCodeBuffer.GetSize() );
}



void    ClangSession::CreateTranslationUnit( void )
{
    if ( m_CxTU )
    {
        // clang parser already exist
        return;
    }

    CXUnsavedFile               unsaved_file = GetCXUnsavedFile();

    m_CxTU = clang_parseTranslationUnit( m_Context.GetCXIndex(), m_SessionName.c_str(), 
                                         static_cast< const char * const *>( m_CFlagsBuffer.GetCFlags() ), m_CFlagsBuffer.GetNumberOfCFlags(), 
                                         &unsaved_file, 1, m_TranslationUnitFlags );
                                         
    clang_reparseTranslationUnit( m_CxTU, 1, &unsaved_file, m_TranslationUnitFlags );
}

void    ClangSession::DeleteTranslationUnit( void )
{
    if ( !m_CxTU )
    {
        // clang parser not exist
        return;
    }

    clang_disposeTranslationUnit( m_CxTU );
    m_CxTU = nullptr;
}



void    ClangSession::Allocate( void )
{
    ReadCFlags();
    ReadSourceCode();
    CreateTranslationUnit();
}

void    ClangSession::Deallocate( void )
{
    DeleteTranslationUnit();
}


void    ClangSession::commandSuspend( void )
{
    DeleteTranslationUnit();
}

void    ClangSession::commandResume( void )
{
    CreateTranslationUnit();
}


void    ClangSession::commandSetCFlags( void )
{
    DeleteTranslationUnit();
    ReadCFlags();
    ReadSourceCode();
    CreateTranslationUnit();
}


void    ClangSession::commandSetSourceCode( void )
{
    ReadSourceCode();
}

void    ClangSession::commandReparse( void )
{
    if ( !m_CxTU )
    {
        // clang parser not exist
        return;
    }

    CXUnsavedFile               unsaved_file = GetCXUnsavedFile();

    clang_reparseTranslationUnit( m_CxTU, 1, &unsaved_file, m_TranslationUnitFlags );
}


void    ClangSession::commandCompletion( void )
{
    if ( !m_CxTU )
    {
        // clang parser not exist
        return;
    }

    Completion                  printer( *this );

    printer.PrintCompleteCandidates();
}


void    ClangSession::commandDiagnostics( void )
{
    if ( !m_CxTU )
    {
        // clang parser not exist
        return;
    }

    Diagnostics                 printer( *this );

    printer.PrintDiagnosticsResult();
}


void    ClangSession::commandDeclaration( void )
{
    if ( !m_CxTU )
    {
        // clang parser not exist
        return;
    }

    Jump                        printer( *this );
    
    printer.PrintDeclarationLocation();
}


void    ClangSession::commandDefinition( void )
{
    if ( !m_CxTU )
    {
        // clang parser not exist
        return;
    }

    Jump                        printer( *this );
    
    printer.PrintDefinitionLocation();
}


void    ClangSession::commandSmartJump( void )
{
    if ( !m_CxTU )
    {
        // clang parser not exist
        return;
    }

    Jump                        printer( *this );
    
    printer.PrintSmartJumpLocation();
}





/*================================================================================================*/
/*  EOF                                                                                           */
/*================================================================================================*/

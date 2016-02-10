/* -*- mode: c++ ; coding: utf-8-unix -*- */
/*  last updated : 2015/07/25.03:32:02 */

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


namespace
{

string  GetNormalizePath( CXFile file )
{
    CXString        filename       = clang_getFileName( file );
    const string    path           = clang_getCString( filename );
    const regex     expression( "[\\\\]+" );
    const string    replace( "/" );
    const string    normalize_path = regex_replace( path, expression, replace );

    clang_disposeString( filename );

    return normalize_path;
}

}



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
        
    void    PrintInclusionFileLocation( void );
    void    PrintDefinitionLocation( void );
    void    PrintDeclarationLocation( void );
    void    PrintSmartJumpLocation( void );

private:
    struct Location
    {
        Location( void ) :
            m_Line( 0 )
            , m_Column( 0 )
        {
        }

        string      m_NormalizePath;
        uint32_t    m_Line;
        uint32_t    m_Column;
    };

    CXCursor    GetCursor( const uint32_t Line, const uint32_t Column ) const;
    void    PrepareTransaction( uint32_t& Line, uint32_t& Column );
    bool    EvaluateCursorLocation( const CXCursor& Cursor );

    bool    EvaluateInclusionFileLocation( void );
    bool    EvaluateDefinitionLocation( void );
    bool    EvaluateDeclarationLocation( void );
    bool    EvaluateSmartJumpLocation( void );
    void    PrintLocation( void );

private:
    ClangSession&       m_Session;
    Location            m_Location;
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



CXCursor    ClangSession::Jump::GetCursor( const uint32_t Line, const uint32_t Column ) const
{
    const CXFile            file     = clang_getFile( m_Session.m_CxTU, m_Session.m_SessionName.c_str() );
    const CXSourceLocation  location = clang_getLocation( m_Session.m_CxTU, file, Line, Column );
    const CXCursor          cursor   = clang_getCursor( m_Session.m_CxTU, location );

    return cursor;
}

void    ClangSession::Jump::PrepareTransaction( uint32_t& Line, uint32_t& Column )
{
    m_Session.m_Reader.ReadToken( "line:%d", Line );
    m_Session.m_Reader.ReadToken( "column:%d", Column );
    
    m_Session.ReadSourceCode();

    CXUnsavedFile       unsaved_file = m_Session.GetCXUnsavedFile();

    clang_reparseTranslationUnit( m_Session.m_CxTU, 1, &unsaved_file, m_Session.m_TranslationUnitFlags );
}


bool    ClangSession::Jump::EvaluateCursorLocation( const CXCursor& Cursor )
{
    if ( clang_isInvalid( Cursor.kind ) )
    {
        return ( false );
    }

    const CXSourceLocation  dest_location = clang_getCursorLocation( Cursor );
    CXFile                  dest_file;
    uint32_t                dest_line;
    uint32_t                dest_column;
    uint32_t                dest_offset;

    clang_getExpansionLocation( dest_location, &dest_file, &dest_line, &dest_column, &dest_offset );

    if ( !dest_file )
    {
        return ( false );
    }

    const string        normalize_path = ::GetNormalizePath( dest_file );
    
    m_Location.m_NormalizePath = normalize_path;
    m_Location.m_Line          = dest_line;
    m_Location.m_Column        = dest_column;

    return ( true );
}


bool    ClangSession::Jump::EvaluateInclusionFileLocation( void )
{
    uint32_t        line;
    uint32_t        column;

    PrepareTransaction( line, column );

    const CXCursor    source_cursor = GetCursor( line, column );

    if ( !clang_isInvalid( source_cursor.kind ) )
    {
        if ( source_cursor.kind == CXCursor_InclusionDirective )
        {
            const CXFile  file = clang_getIncludedFile( source_cursor );

            if ( file )
            {
                const uint32_t  file_line      = 1;
                const uint32_t  file_column    = 1;
                const string    normalize_path = ::GetNormalizePath( file );

                m_Location.m_NormalizePath = normalize_path;
                m_Location.m_Line          = file_line;
                m_Location.m_Column        = file_column;

                return ( true );
            }
        }
    }

    return ( false );
}

bool    ClangSession::Jump::EvaluateDefinitionLocation( void )
{
    uint32_t        line;
    uint32_t        column;

    PrepareTransaction( line, column );

    const CXCursor    source_cursor = GetCursor( line, column );

    if ( !clang_isInvalid( source_cursor.kind ) )
    {
        return EvaluateCursorLocation( clang_getCursorDefinition( source_cursor ) );
    }

    return ( false );
}

bool    ClangSession::Jump::EvaluateDeclarationLocation( void )
{
    uint32_t        line;
    uint32_t        column;

    PrepareTransaction( line, column );

    const CXCursor    source_cursor = GetCursor( line, column );

    if ( !clang_isInvalid( source_cursor.kind ) )
    {
        return EvaluateCursorLocation( clang_getCursorReferenced( source_cursor ) );
    }

    return ( false );
}

bool    ClangSession::Jump::EvaluateSmartJumpLocation( void )
{
    uint32_t        line;
    uint32_t        column;

    PrepareTransaction( line, column );

    const CXCursor    source_cursor = GetCursor( line, column );

    if ( !clang_isInvalid( source_cursor.kind ) )
    {
        if ( source_cursor.kind == CXCursor_InclusionDirective )
        {
            const CXFile  file = clang_getIncludedFile( source_cursor );

            if ( file )
            {
                const uint32_t  file_line      = 1;
                const uint32_t  file_column    = 1;
                const string    normalize_path = ::GetNormalizePath( file );

                m_Location.m_NormalizePath = normalize_path;
                m_Location.m_Line          = file_line;
                m_Location.m_Column        = file_column;

                return ( true );
            }
        }
        else
        {
            return ( EvaluateCursorLocation( clang_getCursorDefinition( source_cursor ) ) 
                     || EvaluateCursorLocation( clang_getCursorReferenced( source_cursor ) ) );
        }
    }

    return ( false );
}


void    ClangSession::Jump::PrintLocation( void )
{
    m_Session.m_Writer.Write( "\"%s\" %d %d ", m_Location.m_NormalizePath.c_str(), m_Location.m_Line, m_Location.m_Column );
}


void    ClangSession::Jump::PrintInclusionFileLocation( void )
{
    if ( EvaluateInclusionFileLocation() )
    {
        PrintLocation();
    }

    m_Session.m_Writer.Flush();
}

void    ClangSession::Jump::PrintDefinitionLocation( void )
{
    if ( EvaluateDefinitionLocation() )
    {
        PrintLocation();
    }

    m_Session.m_Writer.Flush();
}

void    ClangSession::Jump::PrintDeclarationLocation( void )
{
    if ( EvaluateDeclarationLocation() )
    {
        PrintLocation();
    }

    m_Session.m_Writer.Flush();
}

void    ClangSession::Jump::PrintSmartJumpLocation( void )
{
    if ( EvaluateSmartJumpLocation() )
    {
        PrintLocation();
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


void    ClangSession::commandInclusion( void )
{
    if ( !m_CxTU )
    {
        // clang parser not exist
        return;
    }

    Jump                        printer( *this );

    printer.PrintInclusionFileLocation();
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

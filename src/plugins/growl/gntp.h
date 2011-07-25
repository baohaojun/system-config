#ifndef gntp_h
#define gntp_h

//#define CRYPTOPP_ENABLE_NAMESPACE_WEAK 0
#define CRYPTOPP_ENABLE_NAMESPACE_WEAK 1
#include <sstream>
#include <iostream>
#include <string>
#include <vector>
#include <stdexcept>
#include <cryptopp/osrng.h>
#include <cryptopp/files.h>
#include <cryptopp/hex.h>
#include <cryptopp/sha.h>
#include <cryptopp/md5.h>
#include <cryptopp/des.h>
#include <cryptopp/aes.h>
#include <cryptopp/filters.h>
#include <cryptopp/modes.h>

#include <boost/asio.hpp>
#include <boost/thread.hpp>
#include <boost/algorithm/string.hpp>

class gntp {
public:
typedef void (*gntp_callback)(const int& id,const std::string& reason,const std::string& data) ;
private:
class callback_reciver{
public:
    boost::asio::ip::tcp::iostream sock;
    boost::thread *thread;
    gntp_callback callback;
    callback_reciver(std::string host,std::string port,gntp_callback callback):
    sock(host, port),
        thread(NULL),
        callback(callback)
    {}

    ~callback_reciver(){
        if(thread)
            thread->interrupt();
        delete thread;
        sock.close();
    }

    void wait_for_callback(){
        std::string line;
        int id = 0;
        std::string result; 
        std::string data;
        while (std::getline(sock, line)) {
            boost::trim(line);
            //std::cout << "[" << line << "]" << std::endl;				
            if (line.find("Notification-ID: ") == 0) id = atoi(line.substr(17).c_str());
            else if(line.find("Notification-Callback-Result: ") == 0) result = line.substr(30);
            else if(line.find("Notification-Callback-Context: ") == 0) data = line.substr(31);
            else if(line == "\r")break;
        }
        //std::cout << "[id: " << id <<" reason: "<< result << " data: " << data << "]" << std::endl;
        callback(id,result,data);
        delete this;
    }

    void run(){
        if(!callback)
            return;
        thread = new boost::thread( boost::bind(&callback_reciver::wait_for_callback, this) );
    }
};


  static inline std::string to_hex(CryptoPP::SecByteBlock& in) {
    std::string out;
    CryptoPP::HexEncoder hex( NULL, true, 2, "" );
    hex.Attach(new CryptoPP::StringSink(out));
    hex.PutMessageEnd(in.begin(), in.size());
    return out;
  }

  static std::string sanitize_text(std::string name) {
    std::string::size_type n = 0;
    while((n = name.find("\r\n", n)) != std::string::npos)
        name.erase(n, 1);
    return name;
  }

  static std::string sanitize_name(std::string name) {
    std::string::size_type n = 0;
    while((n = name.find("-", n)) != std::string::npos)
        name.erase(n, 1);
    return name;
  }


  static void recv(boost::asio::ip::tcp::iostream& sock) throw (std::runtime_error) {
    std::string error;
    while (1) {
        std::string line;
        if (!std::getline(sock, line)) break;

        //std::cout << "[" << line << "]" << std::endl;
        if (line.find("GNTP/1.0 -ERROR") == 0) error = "unknown error";
        if (line.find("Error-Description: ") == 0) error = line.substr(19);
        if (line == "\r") break;
    }
    if (!error.empty()) throw std::range_error(error);


  }

  callback_reciver * send(const char* method, std::stringstream& stm) throw (std::runtime_error) {
    callback_reciver *cbr = new callback_reciver(hostname_, port_,callback_);
    if (!cbr->sock) throw std::range_error("can't connect to host");

    if (!password_.empty()) {
        // initialize salt and iv
        CryptoPP::SecByteBlock salt(8);
        rng.GenerateBlock(salt.begin(), salt.size());

        // get digest of password+salt hex encoded
        CryptoPP::SecByteBlock passtext(CryptoPP::Weak1::MD5::DIGESTSIZE);
        CryptoPP::Weak1::MD5 hash;
        hash.Update((byte*)password_.c_str(), password_.size());
        hash.Update(salt.begin(), salt.size());
        hash.Final(passtext);
        CryptoPP::SecByteBlock digest(CryptoPP::Weak1::MD5::DIGESTSIZE);
        hash.CalculateDigest(digest.begin(), passtext.begin(), passtext.size());

        cbr->sock << "GNTP/1.0 "
            << method
            << " NONE "
            << " " <<
            sanitize_name(CryptoPP::Weak1::MD5::StaticAlgorithmName())
            << ":" << to_hex(digest) << "." << to_hex(salt)
            << "\r\n"
            << stm.str() << "\r\n\r\n";
    } else {
        cbr->sock << "GNTP/1.0 "
            << method
            << " NONE\r\n"
            << stm.str() << "\r\n";
    }
    recv(cbr->sock);
    return cbr;
  }

  template<class CIPHER_TYPE, class HASH_TYPE>
  callback_reciver *send(const char* method, std::stringstream& stm) throw (std::runtime_error) {
    callback_reciver *cbr = new callback_reciver(hostname_, port_,callback_);
    if (!cbr->sock) throw std::range_error("can't connect to host");

    if (!password_.empty()) {
        // initialize salt and iv
        CryptoPP::SecByteBlock salt(HASH_TYPE::DIGESTSIZE), iv(CIPHER_TYPE::BLOCKSIZE);
        rng.GenerateBlock(salt.begin(), salt.size());
        rng.GenerateBlock(iv.begin(), iv.size());

        // get digest of password+salt hex encoded
        CryptoPP::SecByteBlock passtext(HASH_TYPE::DIGESTSIZE);
        HASH_TYPE hash;
        hash.Update((byte*)password_.c_str(), password_.size());
        hash.Update(salt.begin(), salt.size());
        hash.Final(passtext);
        CryptoPP::SecByteBlock digest(HASH_TYPE::DIGESTSIZE);
        hash.CalculateDigest(digest.begin(), passtext.begin(), passtext.size());

        class CryptoPP::CBC_Mode<CIPHER_TYPE>::Encryption
            encryptor(passtext.begin(), iv.size(), iv.begin());

        std::string cipher_text;
        CryptoPP::StringSource(stm.str(), true,
            new CryptoPP::StreamTransformationFilter(encryptor,
            new CryptoPP::StringSink(cipher_text)
            ) // StreamTransformationFilter
            ); // StringSource

        cbr->sock << "GNTP/1.0 "
            << method
            << " "
            << sanitize_name(CIPHER_TYPE::StaticAlgorithmName())
            << ":" << to_hex(iv)
            << " "
            << sanitize_name(HASH_TYPE::StaticAlgorithmName())
            << ":" << to_hex(digest) << "." << to_hex(salt)
            << "\r\n"
            << cipher_text << "\r\n\r\n";
    } else {
        cbr->sock << "GNTP/1.0 "
            << method
            << " NONE\r\n"
            << stm.str() << "\r\n";
    }
    recv(cbr->sock);
    return cbr;
  }

  void make_regist(std::stringstream& stm, const char* name) {
    stm << "Notification-Name: " << sanitize_text(name) << "\r\n";
    stm << "Notification-Display-Name: " << sanitize_text(name) << "\r\n";
    stm << "Notification-Enabled: True\r\n";
    stm << "\r\n";
  }

  void make_notify(std::stringstream& stm, const char* name, const int id,const char* title, const char* text, const char* icon = NULL, const char* url = NULL,const char *callbackid = NULL) {
    stm << "Application-Name: " << sanitize_text(application_) << "\r\n";
    stm << "Notification-Name: " << sanitize_text(name) << "\r\n";
    stm << "Notification-ID: " << id <<"\r\n";
    if (icon) stm << "Notification-Icon: " << sanitize_text(icon) << "\r\n";
    if (url){
        stm << "Notification-Callback-Target: " << sanitize_text(url) << "\r\n";
    }else if(callbackid){
        stm << "Notification-Callback-Context: "<< sanitize_text(callbackid)  <<"\r\n";
        stm << "Notification-Callback-Context-Type: string\r\n";
    }
    stm << "Notification-Title: " << sanitize_text(title) << "\r\n";
    stm << "Notification-Text: " << sanitize_text(text) << "\r\n";
    stm << "\r\n";
  }

  std::string application_;
  std::string hostname_;
  std::string port_;
  std::string password_;
  std::string icon_;
  CryptoPP::AutoSeededRandomPool rng;
  gntp_callback callback_;

public:

  gntp(std::string application = "gntp-send", std::string icon = "" ,std::string password = "",
     std::string hostname = "localhost", std::string port = "23053") :
  application_(application),
    password_(password),
    hostname_(hostname),
    port_(port),
    icon_(icon),
    callback_(NULL){ }


  void set_gntp_callback(gntp_callback callback){
    callback_ = callback;
  }

  void regist(const char* name) throw (std::runtime_error) {
    std::stringstream stm;
    stm << "Application-Name: " << sanitize_text(application_) << "\r\n";
    stm << "Application-Icon: " << sanitize_text(icon_) <<"\r\n";
    stm << "Notifications-Count: 1\r\n";
    stm << "\r\n";
    make_regist(stm, name);
    callback_reciver *cbr = send("REGISTER", stm);
    delete cbr;
  }

  void regist(const std::vector<std::string> names) throw (std::runtime_error) {
    std::stringstream stm;
    stm << "Application-Name: " << sanitize_text(application_) << "\r\n";
    stm << "Application-Icon: " << sanitize_text(icon_) <<"\r\n";
    stm << "Notifications-Count: " << names.size() << "\r\n";
    stm << "\r\n";
    std::vector<std::string>::const_iterator it;
    for (it = names.begin(); it != names.end(); it++) {
        make_regist(stm, it->c_str());
    }
    callback_reciver *cbr = send("REGISTER", stm);
    delete cbr;
  }

  template<class CIPHER_TYPE, class HASH_TYPE>
  void regist(const char* name) throw (std::runtime_error) {
    std::stringstream stm;
    stm << "Application-Name: " << sanitize_text(application_) << "\r\n";
    stm << "Application-Icon: " << sanitize_text(icon_) <<"\r\n";
    stm << "Notifications-Count: 1\r\n";
    stm << "\r\n";
    make_regist(stm, name);
    callback_reciver *cbr = send<CIPHER_TYPE, HASH_TYPE>("REGISTER", stm);
    delete cbr;
  }

  template<class CIPHER_TYPE, class HASH_TYPE>
  void regist(const std::vector<std::string> names) throw (std::runtime_error) {
    std::stringstream stm;
    stm << "Application-Name: " << sanitize_text(application_) << "\r\n";
    stm << "Application-Icon: " << sanitize_text(icon_) <<"\r\n";
    stm << "Notifications-Count: " << names.size() << "\r\n";
    stm << "\r\n";
    std::vector<std::string>::const_iterator it;
    for (it = names.begin(); it != names.end(); it++) {
        make_regist(stm, it->c_str());
    }
    callback_reciver *cbr = send<CIPHER_TYPE, HASH_TYPE>("REGISTER", stm);
    delete cbr;
  }

  void notify(const char* name,const int id, const char* title, const char* text, const char* icon = NULL, const char* url = NULL,const char *callbackid = NULL) throw (std::runtime_error) {
    std::stringstream stm;
    make_notify(stm, name, id, title, text, icon, url,callbackid);
    callback_reciver *cbr = send("NOTIFY", stm);
    cbr->run();
  }

  template<class CIPHER_TYPE, class HASH_TYPE>
  void notify(const char* name,const int id,  const char* title, const char* text, const char* icon = NULL, const char* url = NULL,const char *callbackid = NULL) throw (std::runtime_error) {
    std::stringstream stm;
    make_notify(stm, name, id, title, text, icon, url,callbackid);
    callback_reciver *cbr = send<CIPHER_TYPE, HASH_TYPE>("NOTIFY", stm);
    cbr->run();
  }
};

#endif

// vim:set et:

#include "filteringmodel.h"
#include <QDebug>
#include <QFont>
#include <QBrush>
#include <lua.hpp>
#include <QSettings>
#include <QProcessEnvironment>
#include <QDir>
#include <QFile>
#include "selectoutput.h"
#include "bhj_help.hpp"
#include <QByteArray>
#include <QPixmap>
#include <algorithm>

FilteringModel::FilteringModel(QObject *parent) :
    QAbstractListModel(parent),
    mSettings("Smartisan", "Wrench", parent),
    mFilter("hello world")
{
    mMaxHistEntries = mSettings.value("max-history-entries", QVariant(20)).toInt();
    L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */
}

int FilteringModel::rowCount(const QModelIndex & /*parent */) const
{
    return mSelectedItems.length();
}

//! [Quoting ModelView Tutorial]
// mymodel.cpp
QVariant FilteringModel::data(const QModelIndex &index, int role) const
{
    int row = index.row();

    switch(role) {
    case Qt::DisplayRole:
        return mSelectedItems[row].displayText;
        break;
    case Qt::DecorationRole:
        if (1) { // for the key declaration
            if (row >= mSelectedItems.size())
                return QVariant();
            return mSelectedItems[row].icon;
        }
    }
    return QVariant();
}

void FilteringModel::sortEntriesWithHistory(int oldRows)
{
    foreach(const QString& history, mHistoryList) {
        if (mSelectedItemsRevMap.contains(history)) {
            mSelectedItems.removeOne(mSelectedItemsRevMap[history]);
            mSelectedItems.push_front(mSelectedItemsRevMap[history]);
        }
    }
    this->dataChanged(index(0, 0), index(oldRows || mSelectedItems.size(), 0));
}

void FilteringModel::setFilter(QString filter)
{
    if (!mInitFilter.isEmpty()) {
        filter = filter + " " + mInitFilter;
    }
    if (filter == mFilter) {
        sortEntriesWithHistory();
        return;
    }
    mFilter = filter;
    int error = luaL_loadstring(L, "wrench = require('wrench')") || lua_pcall(L, 0, 0, 0);
    lua_getglobal(L, "wrench");
    lua_getfield(L, -1, "split");
    lua_pushstring(L, " ");
    lua_pushstring(L, filter.toUtf8().constData());
    error = lua_pcall(L, 2, 1, 0);
    if (error) {
         qDebug() << "Error calling split:" << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        return;
    }
    int nOldRows = mSelectedItems.length();
    mSelectedItems.clear();

    QStringList split;
    int n = luaL_len(L, -1);
    for (int i = 1; i <= n; i++) {
        lua_rawgeti(L, -1, i);
        split << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        lua_settop(L, -2);
    }
    lua_settop(L, 0);

    mSelectedItemsRevMap.clear();
    filterSelectedItems(split);

    sortEntriesWithHistory(nOldRows);
}

QString FilteringModel::getSelectedText(int i)
{

    if (i >= 0 && i < mSelectedItems.length()) {
        SelectedItem si = mSelectedItems[i];
        return si.selectedText;
    }
    return "";
}

QString FilteringModel::getSelectedDisplayText(int i)
{

    if (i >= 0 && i < mSelectedItems.length()) {
        SelectedItem si = mSelectedItems[i];
        return si.displayText;
    }
    return "";
}

void FilteringModel::updateHistory(int i)
{
    if (i >= 0 && i < mSelectedItems.size()) {
        QString key = mSelectedItems[i].displayText;
        updateHistory(key);
        for (int i = 0; i < mHistoryList.size(); i++) {
            mSettings.setValue(getNthHistoryVarName(i), QVariant(mHistoryList[i]));
        }
    }
}

void FilteringModel::updateHistory(QString key)
{
    if (key.isEmpty()) {
        return;
    }
    while (mHistoryList.removeOne(key)) {
        ;
    }
    mHistoryList.push_back(key);
    while (mHistoryList.size() > mMaxHistEntries) {
        mHistoryList.pop_front();
    }
}

void FilteringModel::initHistory()
{
    for (int j = 0; j < mMaxHistEntries; j++) {
        updateHistory(mSettings.value(getNthHistoryVarName(j), QVariant("")).toString());
    }
    setFilter("");
}

QString FilteringModel::getNthHistoryVarName(int n)
{
    return getHistoryName() + QString().sprintf("-%d", n % mMaxHistEntries);
}
QString FilteringModel::getHistoryHeadName()
{
    return getHistoryName() + "-head";
}

#include <windows.h>

BOOL svc_start_dispatcher(LPTSTR name, LPSERVICE_MAIN_FUNCTION proc)
{
    SERVICE_TABLE_ENTRY table[2];

    table[0].lpServiceName = name;
    table[0].lpServiceProc = proc;

    table[1].lpServiceName = NULL;
    table[1].lpServiceProc = NULL;

    return StartServiceCtrlDispatcher(table);
}

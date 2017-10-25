#include <CL/cl.h>
#include <vector>
#include <iostream>
#include <string>
#include <fstream>
#include <streambuf>
#include <Windows.h>
#include <atlbase.h>
#include <atlconv.h>

#define IMG_WIDTH 1280
#define IMG_HEIGHT 720

#define KERNEL_FILE "raytracer.cl"

//#define IMG_WIDTH 752
//#define IMG_HEIGHT 480

const TCHAR g_szClassName[] = L"(╯°□°)╯︵ ┻━┻";
cl_int *result;
bool running = true;
cl_int iteration = 0;

std::string loadKernel(const char *name) {
	std::ifstream in(name);
	std::string result((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
	return result;
}

cl_program createProgram(const std::string *source, cl_context context, cl_int *error) {
	size_t lengths[1] = {source->size()};
	const char *sources[1] = {source->data()};

	cl_program program = clCreateProgramWithSource(context, 1, sources, lengths, error);
	return program;
}

void display_error(HWND hwnd, TCHAR *message) {
	MessageBox(
		hwnd,
		(LPCWCHAR) message,
		L"Error!",
		MB_ICONEXCLAMATION | MB_OK);
}

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch (msg)
	{
	case WM_QUIT:
		running = false;
		break;
	case WM_CLOSE:
		running = false;
		DestroyWindow(hwnd);
		break;
	case WM_DESTROY:
		running = false;
		PostQuitMessage(0);
		break;
	case WM_ERASEBKGND:
		break;
	case WM_PAINT: {
		if (result) {
			PAINTSTRUCT ps;
			HDC hdc = BeginPaint(hwnd, &ps);
			HBITMAP bmp = CreateBitmap(IMG_WIDTH, IMG_HEIGHT, 1, 32, (void*)result);
			HDC src = CreateCompatibleDC(hdc);
			SelectObject(src, bmp);
			BitBlt(hdc, 0, 0, IMG_WIDTH, IMG_HEIGHT, src, 0, 0, SRCCOPY);
			DeleteDC(src);
			DeleteObject(bmp);
			EndPaint(hwnd, &ps);
		}
		return DefWindowProc(hwnd, msg, wParam, lParam);
	} break;
	default:
		return DefWindowProc(hwnd, msg, wParam, lParam);
	}
	return 0;
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {

	WNDCLASSEX wc;
	HWND hwnd;
	MSG Msg;

	wc.cbSize = sizeof(WNDCLASSEX);
	wc.style = 0;
	wc.lpfnWndProc = WndProc;
	wc.cbClsExtra = 0;
	wc.cbWndExtra = 0;
	wc.hInstance = hInstance;
	wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
	wc.hCursor = LoadCursor(NULL, IDC_ARROW);
	wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
	wc.lpszMenuName = NULL;
	wc.lpszClassName = (LPCWCHAR) g_szClassName;
	wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);

	if (!RegisterClassEx(&wc)) {
		display_error(hwnd, L"Window Registration Failed!");
		return 1;
	}

	hwnd = CreateWindowEx(
		WS_EX_CLIENTEDGE,
		(LPCWCHAR) g_szClassName,
		(LPCWCHAR) L"¯\\_(ツ)_/¯",//L"💩💩💩💩💩💩💩",//L"(╯°□°)╯︵ ┻━┻",
		WS_OVERLAPPEDWINDOW,
		CW_USEDEFAULT, CW_USEDEFAULT, IMG_WIDTH, IMG_HEIGHT,
		NULL, NULL, hInstance, NULL);

	if (hwnd == NULL) {
		display_error(hwnd, L"Window Creation Failed!");
		return 1;
	}

	ShowWindow(hwnd, nCmdShow);
	UpdateWindow(hwnd);	


	cl_uint platform_id_count = 0;
	clGetPlatformIDs(0, nullptr, &platform_id_count);

	if (0 == platform_id_count) {
		display_error(hwnd, L"No platforms found.");
		return 1;
	}

	std::vector<cl_platform_id> platform_ids(platform_id_count);
	clGetPlatformIDs(platform_id_count, platform_ids.data(), nullptr);

	for (auto platform : platform_ids) {
		size_t platform_name_length = 0;
		clGetPlatformInfo(platform, CL_PLATFORM_NAME, 0, nullptr, &platform_name_length);
		char *platform_name = (char*)malloc(sizeof(char) * platform_name_length);
		clGetPlatformInfo(platform, CL_PLATFORM_NAME, platform_name_length, platform_name, nullptr);

		//display_error(hwnd, CA2W(platform_name));
		free(platform_name);
	}

	cl_uint device_id_count = 0;
	clGetDeviceIDs(platform_ids[0], CL_DEVICE_TYPE_ALL, 0, nullptr, &device_id_count);
	
	if (0 == device_id_count) {
		display_error(hwnd, L"No devices found.");
		return 1;
	}
	
	std::vector<cl_device_id> device_ids(device_id_count);
	clGetDeviceIDs(platform_ids[0], CL_DEVICE_TYPE_ALL, device_id_count, device_ids.data(), nullptr);

	for (auto device : device_ids) {
		size_t device_name_length = 0;
		clGetDeviceInfo(device, CL_DEVICE_NAME, 0, nullptr, &device_name_length);
		char *device_name = (char*)malloc(sizeof(char) * device_name_length);
		clGetDeviceInfo(device, CL_DEVICE_NAME, device_name_length, device_name, nullptr);

		//display_error(hwnd, CA2W(device_name));
		free(device_name);
	}

	const cl_context_properties context_properties[] = {
		CL_CONTEXT_PLATFORM,
		reinterpret_cast<cl_context_properties> (platform_ids[0]),
		0, 0
	};

	cl_int error = 0;
	cl_context context = clCreateContext(
		context_properties, 
		device_id_count,
		device_ids.data(), 
		nullptr,
		nullptr, 
		&error);

	if (error != CL_SUCCESS) {
		display_error(hwnd, L"Error creating a context.");
		return 1;
	}

	std::string source = loadKernel(KERNEL_FILE);
	
	if (0 == source.size()) {
		display_error(hwnd, L"Error reading kernel file.");
		return 1;
	}

	cl_program program = createProgram(&source, context, &error);

	if (error != CL_SUCCESS) {
		display_error(hwnd, L"Error creating a program.");
		return 1;
	}

	error = clBuildProgram(program, 1, device_ids.data(), nullptr, nullptr, nullptr);

	if (error != CL_SUCCESS) {
		if (error == CL_BUILD_PROGRAM_FAILURE) {
			size_t log_size;
			clGetProgramBuildInfo(program, device_ids[0], CL_PROGRAM_BUILD_LOG, 0, NULL, &log_size);

			char *log = (char *)malloc(log_size);
			clGetProgramBuildInfo(program, device_ids[0], CL_PROGRAM_BUILD_LOG, log_size, log, NULL);

			display_error(hwnd, CA2W(log));
		}
		display_error(hwnd, L"Error building the program.");
		return 1;
	}

	cl_kernel kernel = clCreateKernel(program, "MAIN", &error);

	if (error != CL_SUCCESS) {
		display_error(hwnd, L"Error creating a kernel.");
		return 1;
	}

	const cl_int img_width = IMG_WIDTH;
	const cl_int img_height = IMG_HEIGHT;
	const cl_int num_pixels = img_width * img_height;
	
	result = (cl_int*)malloc(sizeof(cl_int) * num_pixels);

	if (nullptr == result) {
		display_error(hwnd, L"Failed to allocate result array.");
		return 1;
	}

	cl_mem buffer_integration = clCreateBuffer(context, CL_MEM_READ_WRITE | CL_MEM_ALLOC_HOST_PTR, sizeof(cl_float3) * num_pixels, nullptr, &error);

	if (error != CL_SUCCESS) {
		display_error(hwnd, L"Error creating integration buffer.");
		return 1;
	}

	cl_mem buffer_result = clCreateBuffer(context, CL_MEM_WRITE_ONLY | CL_MEM_COPY_HOST_PTR, sizeof(cl_int) * num_pixels, result, &error);

	if (error != CL_SUCCESS) {
		display_error(hwnd, L"Error creating result buffer.");
		return 1;
	}

	cl_command_queue queue = clCreateCommandQueue(context, device_ids[0], 0, &error);

	if (error != CL_SUCCESS) {
		display_error(hwnd, L"Error creating a queue.");
		return 1;
	}

	clSetKernelArg(kernel, 0, sizeof(cl_int), &img_width);
	clSetKernelArg(kernel, 1, sizeof(cl_int), &img_height);
	clSetKernelArg(kernel, 2, sizeof(cl_mem), &buffer_integration);
	clSetKernelArg(kernel, 3, sizeof(cl_mem), &buffer_result);

	const size_t globalWorkSize[] = {num_pixels, 0, 0};

	while (running) {
		iteration++;
		while (PeekMessage(&Msg, NULL, 0, 0, PM_REMOVE) > 0) {
			TranslateMessage(&Msg);
			DispatchMessage(&Msg);
		}

		clSetKernelArg(kernel, 4, sizeof(cl_int), &iteration);

		error = clEnqueueNDRangeKernel(queue, kernel, 1, nullptr, globalWorkSize, nullptr, 0, nullptr, nullptr);

		if (error != CL_SUCCESS) {
			display_error(hwnd, L"Error pushing kernel into the queue.");
			return 1;
		}

		error = clEnqueueReadBuffer(queue, buffer_result, CL_TRUE, 0, sizeof(cl_int) * num_pixels, result, 0, nullptr, nullptr);

		if (error != CL_SUCCESS) {
			display_error(hwnd, L"Error retrieving results.");
			return 1;
		}

		InvalidateRect(hwnd, nullptr, true);
		SetWindowText(hwnd, CA2W(std::to_string(iteration).c_str()));
	}

	clReleaseCommandQueue(queue);
	clReleaseMemObject(buffer_result);
	clReleaseMemObject(buffer_integration);
	clReleaseKernel(kernel);
	clReleaseProgram(program);
	clReleaseContext(context);

	return Msg.wParam;
}
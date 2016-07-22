// NeuralNetwork.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include<algorithm>
#include<cassert>
#include<cmath>
#include<fstream>
#include<iomanip>
#include<iostream>
#include<list>
#include<memory>
#include<random>
#include<string>
#include<vector>

// TODO: Implement Copy-on-Write.

class DimensionMismatchException : public std::exception
{
public:
	DimensionMismatchException() {}
};

template<typename T> class Matrix;
template<typename T> class Network;
template<typename T>
std::ostream &operator<< (std::ostream& o, const Matrix<T>& mat);

template<typename T>
class Matrix {
public:
	explicit Matrix(unsigned nrows, unsigned ncols)
		: nrows_(nrows)
		, ncols_(ncols)
		, copyonwrite_(false)
		, mempool_(new T [nrows*ncols*sizeof(T)])
		, data_(mempool_.get()) {
	}

	~Matrix() {
		data_ = nullptr;
		mempool_.reset();
	}

	inline void copy() {
		if (!copyonwrite_)
			return;
		std::shared_ptr<T> oldmem = mempool_;
		T* old_data = data_;
		mempool_.reset(new T[size()]);
		memcpy(mempool_.get(), old_data, size());
		data_ = mempool_.get();
		copyonwrite_ = false;
	}

	Matrix(const Matrix<T>& mat) 
	: nrows_(0)
	, ncols_(0)
	, copyonwrite_(false)
	, mempool_(nullptr)
	, data_(nullptr) {
		*this = mat;
	}

	Matrix<T>& operator=(const Matrix<T>& mat) {
		if (this != &mat) {
			nrows_ = mat.nrows();
			ncols_ = mat.ncols();
			if (copyonwrite_) {
				mempool_ = mat.mempool_;
				data_ = mat.data_;
			} else {
				mempool_.reset(new T[mat.nrows() * mat.ncols() * sizeof(T)]);
				memcpy(mempool_.get(), mat.mempool_.get(), mat.size());
				data_ = mempool_.get();
			}
		}
		return *this;
	}

	// move assignment
	Matrix<T>& operator=(Matrix<T>&& mat) {
		if (this != &mat) {
			nrows_ = mat.nrows();
			ncols_ = mat.ncols();
			mempool_ = mat.mempool_;
			data_ = mat.data_;
			mat.mempool_.reset();
			mat.data_ = nullptr;
		}
		return *this;
	}

	inline T& operator () (unsigned r, unsigned c) {
#ifdef _DEBUG
		if (r >= nrows_ || c >= ncols_)
			throw DimensionMismatchException();
#endif
		return data_[r * ncols_ + c];
	}

	inline const T& operator () (unsigned r, unsigned c) const {
#ifdef _DEBUG
		if (r >= nrows_ || c >= ncols_)
			throw DimensionMismatchException();
#endif
		return data_[r * ncols_ + c];
	}

	inline T* begin() { return &(*this)(0, 0); }
	inline const T* begin() const { return &(*this)(0, 0); }
	inline T* end() { return &(*this)(nrows() - 1, ncols() - 1) + 1; }
	inline const T* end() const { return &(*this)(nrows() - 1, ncols() - 1) + 1; }
	inline const T* data() const { return &(*this)(0, 0); }

	// tranposed.
	Matrix<T> Tr() const {
		Matrix<T> t(ncols(), nrows());
		for (unsigned r = 0; r < nrows(); ++r) {
			for (unsigned c = 0; c < ncols(); ++c) {
				t(c, r) = (*this)(r, c);
			}
		}
		return t;
	}

	Matrix<T> row(unsigned r) const {
#ifdef _DEBUG
		if ( r >= nrows() )
			throw DimensionMismatchException();
#endif
		Matrix<T> rs(1, ncols());
		memcpy(rs.data_, (const void *)(data_ + r * ncols_), sizeof(T)*ncols());
		return rs;
	}

	Matrix<T> cols(unsigned start, unsigned end) const {
#ifdef _DEBUG
		if (start > end
			|| start >= ncols()
			|| end >= ncols())
			throw DimensionMismatchException();
#endif
		Matrix<T> cl(nrows(), end - start + 1);
		for (unsigned r = 0; r < cl.nrows(); ++r) {
			for (unsigned c = 0; c < cl.ncols(); ++c) {
				cl(r, c) = (*this)(r, c+start);
			}
		}
		return cl;
	}

	Matrix<T> operator+(const Matrix<T> &mat) const {
#ifdef _DEBUG
		if (ncols() != mat.ncols() || nrows() < mat.nrows())
			throw DimensionMismatchException();
#endif
		Matrix<T> added(nrows(), ncols());
		for (unsigned r = 0; r < added.nrows(); ++r) {
			for (unsigned c = 0; c < added.ncols(); ++c) {
				added(r, c) = (*this)(r, c) + mat(r%mat.nrows(), c);
			}
		}
		return added;
	}

	Matrix<T> &operator+=(const Matrix<T> &mat) {
#ifdef _DEBUG
		if (ncols() != mat.ncols() || nrows() != mat.nrows())
			throw DimensionMismatchException();
#endif
		T* p = const_cast<Matrix<float> &>(mat).begin();
		for (auto& e : *this) e += *p++; 
		return (*this);
	}

	Matrix<T> operator-(const Matrix<T> &mat) const {
#ifdef _DEBUG
		if (ncols() != mat.ncols() || nrows() < mat.nrows())
			throw DimensionMismatchException();
#endif
		Matrix<T> added(nrows(), ncols());
		for (unsigned r = 0; r < added.nrows(); ++r) {
			for (unsigned c = 0; c < added.ncols(); ++c) {
				added(r, c) = (*this)(r, c) - mat(r%mat.nrows(), c);
			}
		}

		return added;
	}

	Matrix<T> &operator-=(const Matrix<T> &mat) {
#ifdef _DEBUG
		if (ncols() != mat.ncols() || nrows() != mat.nrows())
			throw DimensionMismatchException();
#endif
		T* p = const_cast<Matrix<float> &>(mat).begin();
		for (auto& e : *this) e -= (*p++);
		return (*this);
	}

	Matrix<T> &operator-=(float m) {
		for (auto& e : *this) e -= m;
		return (*this);
	}

	inline Matrix<T> operator-(float m) const {
		Matrix<T> added(*this);
		for (T& e : added) e -= m;
		return added;
	}

	Matrix<T> operator*(const Matrix<T> &mat) const {
#ifdef _DEBUG
		if (ncols() != mat.ncols() || nrows() != mat.nrows())
			throw DimensionMismatchException();
#endif
		Matrix<T> multed(mat);
		const T *p = data();
		for (T& e : multed) e *= *p++;
		return multed;
	}

	inline Matrix<T> operator*(float m) const {
		Matrix<T> multed(*this);
		for (auto& e : multed) e *= m;
		return multed;
	}

	Matrix<T> &operator*=(float m) {
		for (auto& e : *this) e *= m;
		return (*this);
	}

	Matrix<T> dot(const Matrix<T> &mat) const {
#ifdef _DEBUG
		if (ncols() != mat.nrows())
			throw DimensionMismatchException();
#endif
		Matrix<T> dotted(nrows(), mat.ncols());
		for (unsigned r = 0; r < dotted.nrows(); ++r) {
			for (unsigned c = 0; c < dotted.ncols(); ++c) {
				T sum = 0;
				for (unsigned k = 0; k < ncols_; ++k) {
					sum += (*this)(r, k) * mat(k, c);
				}
				dotted(r, c) = sum;
			}
		}
		return dotted;
	}

	inline Matrix<T> &zeros() {
		for (auto& e : *this) e = (T) 0;
		return *this;
	}

	inline unsigned nrows() const { return nrows_;  }
	inline unsigned ncols() const { return ncols_; }
	inline unsigned size() const { return nrows()*ncols()*sizeof(T); }

	inline std::string shape() const {
		return std::to_string(nrows())
			+ std::string("x") 
			+ std::to_string(ncols());
	}

	friend std::ostream &operator<< <>(std::ostream& o, const Matrix<T>& mat);

	void load(const std::string &path) {
		std::ifstream ifs(path, std::ifstream::in | std::ifstream::binary);
		unsigned sz = nrows() * ncols() * sizeof(T);
		ifs.seekg(0x50);
		ifs.read((char *) data_, sz);
		ifs.close();
	}

	void save(const std::string &path) const {
		std::ofstream ofs(path, std::ofstream::out | std::ofstream::binary);
		unsigned sz = nrows() * ncols() * sizeof(T);
		unsigned char dummy[0x50];
		ofs.write((char *) dummy, 0x50);
		ofs.write((char *)(data()), sz);
		ofs.close();
	}

private:
	Matrix() = delete;

	unsigned nrows_;
	unsigned ncols_;
	bool copyonwrite_;
	std::shared_ptr<T> mempool_;
	// data_ points to a location in mempool_.
	T* data_;
};

template<typename T>
std::ostream &operator<< (std::ostream& o, const Matrix<T>& mat) {
	for (unsigned r = 0; r < mat.nrows_; ++r) {
		for (unsigned c = 0; c < mat.ncols_; ++c) {
			o << std::setw(7) << std::setprecision(2) << mat(r, c);
		}
		o << std::endl;
	}
	return o << std::endl;
}

template<typename T>
Matrix<T> operator*(float lhs, Matrix<T> &rhs) {
	return rhs * lhs;
}

template<typename T>
void random(Matrix<T> &mat) {
	std::random_device rd;
	std::mt19937 mt(rd());
	std::uniform_int_distribution<T> dist(0, 9);
	for (auto& e : mat) e = dist(mt);
}

template<>
void random(Matrix<float> &mat) {
	std::random_device rd;
	std::mt19937 mt(rd());
	std::uniform_real_distribution<float> dist(-5.0f, 5.0f);
	for (auto& e : mat) e = dist(mt);
}

template<>
void random(Matrix<unsigned char> &mat) {
	std::random_device rd;
	std::mt19937 mt(rd());
	std::uniform_int_distribution<int> dist(0, 10);
	for (auto& e : mat) e = (unsigned char)dist(mt);
}

template<typename T>
class Cost {
public:
	virtual Matrix<T> delta(const Matrix<T>& z, const Matrix<T>& a, const Matrix<T>&y) {
		return cost_derivative(a, y) * sigmoid_prime(z);
	}

	Matrix<T> sigmoid_prime(const Matrix<T> &mat) const {
		Matrix<T> prime(mat);
		for (auto& e : prime) e = sigmoid_prime(e);
		return prime;
	}

	Matrix<T> sigmoid(const Matrix<T> &mat) const {
		Matrix<T> ret(mat);
		for (auto& e : ret) e = (T) sigmoid(e);
		return ret;
	}

	inline float sigmoid(float z) const {
		float a = float(1.0 / (1.0 + exp(-1.0*z)));
#ifdef _DEBUG
		if (a > 1.0f || a < 0.0f) {
			std::abort();
		}
#endif
		return a;
	}

protected:
	inline float sigmoid_prime(float z) const {
		float sig_z = sigmoid(z);
		float prime_sig = sig_z * (1.0f - sig_z);
#ifdef _DEBUG
		if (prime_sig > 1.0f || prime_sig < 0.0f)
			std::abort();
#endif
		return prime_sig;
	}

	Matrix<T> cost_derivative(const Matrix<T> &output_activation, const Matrix<T> &y) const {
		return output_activation - y;
	}
};

template<typename T>
class CrossEntropyCost : public Cost<T> {
public:
	virtual Matrix<T> delta(const Matrix<T>& z, const Matrix<T>& a, const Matrix<T>&y) {
		return a - y;
	}
};

template<typename T>
class Network {
public:
	explicit Network(const std::vector<unsigned>& sizes) throw()
		: sizes_(sizes)
		, biases_()
		, weights_()
		, cost_(std::make_unique<CrossEntropyCost<T>>()) {
		std::cout << "sizes: ";
		for (auto i : sizes)
			std::cout << i << " ";
		std::cout << std::endl;
		// first one is dummy
		for (std::vector<unsigned>::iterator y = sizes_.begin(); y != sizes_.end(); ++y) {
			Matrix<float> mat(*y, 1);
			biases_.push_back(mat);
			std::cout << "biases=" << 1 << "x" << *y << std::endl;
		}
		{
			// dummy for input
			Matrix<float> mat(sizes_[0], 1);
			weights_.push_back(mat);
		}
		for (std::vector<unsigned>::iterator y = sizes_.begin() + 1; y != sizes_.end(); ++y) {
			Matrix<float> mat(*y, *(y - 1));
			weights_.push_back(mat);
			std::cout << "weights=" << *(y - 1) << "x" << *y << std::endl;
		}
		//init_random();
		init_from_file();
	}

	void sqrt(Matrix<float> &mat) {
		for (auto& e : mat) e = sqrt(e);
	}

	void init_random() {
		random(weights_[1]);
		sqrt(weights_[1]);
		random(weights_[2]);
		sqrt(weights_[2]);
		random(biases_[1]);
		random(biases_[2]);
	}

	void init_from_file() {
		weights_[1].load("c:/temp/w0.npy");
		weights_[2].load("c:/temp/w1.npy");
		biases_[1].load("c:/temp/b0.npy");
		biases_[2].load("c:/temp/b1.npy");
	}

	void load_data(const std::string &path, Matrix<float> &img) const {
		std::ifstream ifs(path, std::ifstream::in | std::ifstream::binary);
		unsigned sz = img.nrows() * img.ncols() * sizeof(float);
		ifs.seekg(0x50);
		ifs.read(reinterpret_cast<char *>(&img(0, 0)), sz);
		ifs.close();
	}

	virtual ~Network(){
		weights_[1].save("c:/temp/w0.my");
		weights_[2].save("c:/temp/w1.my");
		biases_[1].save("c:/temp/b0.my");
		biases_[2].save("c:/temp/b1.my");
	}

	void SGD(const Matrix<T>& training_data, 
		const Matrix<T>& labels, 
		unsigned epochs, 
		unsigned mini_batch_size, 
		float eta, 
		float lmbda, 
		const Matrix<T>& test_data, const Matrix<T>& test_label) {
		unsigned n = training_data.nrows();
		std::srand(unsigned(std::time(0)));
		std::vector<unsigned> index(n);
		for (unsigned i = 0; i < n; ++i)
			index[i] = i;
		for (unsigned e = 0; e < epochs; ++e) {
			std::random_shuffle(index.begin(), index.end());
			// training_data
			for (unsigned k = 0; k < n; k += mini_batch_size) {
				unsigned mb_size = (n - k) > mini_batch_size ? mini_batch_size : (n - k);
				update_mini_batch(training_data, labels, index, k, mb_size, eta, lmbda, float(n));
			}
			std::cout << "epoch: " << e << std::endl;
			std::cout << "Training score=" << evaluate(training_data, labels)
				<< "/" << training_data.nrows()
				<< std::endl;
			std::cout << "Test score=" << evaluate(test_data, test_label) 
					<< "/" << test_data.nrows() 
					<< std::endl;
		}
	}

	void update_mini_batch
	(const Matrix<T>& mb_x, const Matrix<T>& mb_y, 
		const std::vector<unsigned> &index, unsigned k, 
		unsigned mb_size, float eta, float lmbda, float n) const {
		std::vector<Matrix<T>> nabla_b = zeros(biases_);
		std::vector<Matrix<T>> nabla_w = zeros(weights_);
		for (unsigned i = 0; i < mb_size; ++i) {
			std::pair<std::vector<Matrix<T>>, std::vector<Matrix<T>>> delta_nabla =
				backprop(mb_x.row(index[k+i]), mb_y.row(index[k+i]));
			for (unsigned j = 1; j < weights_.size(); ++j) {
				nabla_b[j] += delta_nabla.first[j];
				nabla_w[j] += delta_nabla.second[j];
			}
			float learning_rate = (eta / mb_size);

			//self.weights = [(1 - eta*(lmbda / n))*w - (eta / len(mini_batch))*nw
			//	for w, nw in zip(self.weights, nabla_w)]
			//self.biases = [b - (eta / len(mini_batch))*nb
			//		for b, nb in zip(self.biases, nabla_b)]

			// skip input layer
			for (unsigned j = 1; j < weights_.size(); ++j) {
				const_cast<Matrix<T>&>(biases_[j]) -= (learning_rate * nabla_b[j]);
				Matrix<T>& w  = const_cast<Matrix<T>&>(weights_[j]);
				w = (1.0f - eta * (lmbda / n))*w - (learning_rate * nabla_w[j]);
			}
		}
	}

	// Return a tuple (nabla_b, nabla_w) representing the
	// gradient for the cost function Cx. *nabla_b* and
	// *nabla_w* are layer - by -layer lists of Matrix,
	// similar to biases_ and weights_.
	std::pair<std::vector<Matrix<T>>, std::vector<Matrix<T>>> 
	backprop(const Matrix<T>& feature, const Matrix<T>& label) const {
		std::vector<Matrix<T>> nabla_b = zeros(biases_);
		std::vector<Matrix<T>> nabla_w = zeros(weights_);
		//
		// feed forward
		//
		Matrix<T> activation = feature.Tr();
		// store all layer's activations
		std::vector<Matrix<T>> activations;
		activations.push_back(activation);
		// store all z vectors
		// z = w.a + b
		std::vector<Matrix<T>> zs;
		zs.push_back(activation);
		for (unsigned i = 1; i < weights_.size(); ++i) {
			Matrix<T> z = weights_[i].dot(activation) + biases_[i];
			zs.push_back(z);
			activation = cost_->sigmoid(z);
			activations.push_back(activation);
		}
		//
		// backward
		//
		Matrix<T> y = label.Tr();
		Matrix<T> delta = cost_->delta(zs.back(), activations.back(), y);
		nabla_b.back() = delta;
		nabla_w.back() = delta.dot(activations[activations.size() - 2].Tr());
		// hidden layers
		// L-1, L-2, ..., 2
		for (size_t layer = sizes_.size() - 2; layer > 0; --layer) {
			Matrix<T> z = zs[layer];
			Matrix<T> sp = cost_->sigmoid_prime(z);
			delta = weights_[layer + 1].Tr().dot(delta) * sp;
			nabla_b[layer] = delta;
			nabla_w[layer] = delta.dot(activations[layer - 1].Tr());
		}
		return std::make_pair(nabla_b, nabla_w);
	}

	int evaluate(const Matrix<T> &testImages, const Matrix<T> &testLabels) {
		int sum = 0;
		for (unsigned r = 0; r < testImages.nrows(); ++r) {
			Matrix<T> row = testImages.row(r);
			Matrix<T> testPredicted = feedForward(row);
			unsigned p = argmax(testPredicted);
			unsigned y = argmax(testLabels.row(r));
			sum += ((p == y) ? 1 : 0);
		}
		return sum;
	}

	void dump() {
		std::cout << "network" << std::endl;
		std::cout << "weights[2]=" << weights_[2] << std::endl;
		std::cout << "biases[2]=" << biases_[2] << std::endl;
	}

private:
	Network() = delete;

	int argmax(const Matrix<T> &vec) const {
		assert(vec.ncols() == 1 || vec.nrows()==1);

		int idx = 0;
		int max_i = 0;
		T max_v = vec(0, 0);
		for (const auto& e : vec) {
			if (e > max_v) {
				max_v = e;
				max_i = idx;
			}
			++idx;
		}

		return max_i;
	}

	Matrix<T> feedForward(const Matrix<T> &input) const {
		assert(input.nrows() == 1);
		Matrix<T> features = input.Tr();
		size_t len = weights_.size();
		// skip input layer
		for (size_t idx = 1; idx < len; ++idx) {
			features = cost_->sigmoid(weights_[idx].dot(features) + biases_[idx]);
		}
		return features;
	}

	std::vector<Matrix<float>> zeros(const std::vector<Matrix<float>>& input) const {
		std::vector<Matrix<float>> zs;
		for (const auto& i : input) {
			Matrix<float> a(i.nrows(), i.ncols());
			zs.push_back(a.zeros());
		}
		return zs;
	}

	std::vector<unsigned> sizes_;
	std::vector<Matrix<T>> biases_;
	std::vector<Matrix<T>> weights_;
	std::unique_ptr<Cost<T>> cost_;
};

class Mnist {
public:
	Mnist() throw()
		: trainImages_(60000, 28*28) 
		, trainLabels_(60000, 10)
		, testImages_(10000, 28*28)
		, testLabels_(10000, 10)
	{
		load_data(train_images, trainImages_);
		load_data(train_labels, trainLabels_);
		load_data(test_images, testImages_);
		load_data(test_labels, testLabels_);
	}

	inline const unsigned imgSize() const { return 28 * 28; }

	inline Matrix<float> &trainImages() { return trainImages_; }
	inline Matrix<float> &trainLabels() { return trainLabels_; }
	inline Matrix<float> &testImages() { return testImages_; }
	inline Matrix<float> &testLabels() { return testLabels_; }

private:
	void load_data(const std::string &path, Matrix<float> &img) {
		std::ifstream ifs(path, std::ifstream::in | std::ifstream::binary);
		bool bImage = false;
		if (path.compare(train_images) == 0 || path.compare(test_images) == 0)
			bImage = true;
		unsigned sz = img.nrows()*img.ncols();
		if (bImage)
			ifs.seekg(16);
		else {
			ifs.seekg(8);
			sz = img.nrows();
		}
		std::unique_ptr<unsigned char []> data(new unsigned char [sz]);
		ifs.read((char *)data.get(), sz);
		ifs.close();
		if (bImage)
			load_img(img, sz, data.get());
		else
			load_label(img, sz, data.get());
	}

	void load_img(Matrix<float> &img, unsigned sz, unsigned char *data) {
		float *idata = &img(0, 0);
		for (unsigned i = 0; i < sz; ++i)
			idata[i] = float(data[i])/256.f;
	}

	void load_label(Matrix<float> &label, unsigned sz, unsigned char *data) {
		float *idata = &label(0, 0);
		for (unsigned i = 0; i < sz; ++i) {
			idata[i * 10 + 0] = 0.0f;
			idata[i * 10 + 1] = 0.0f;
			idata[i * 10 + 2] = 0.0f;
			idata[i * 10 + 3] = 0.0f;
			idata[i * 10 + 4] = 0.0f;
			idata[i * 10 + 5] = 0.0f;
			idata[i * 10 + 6] = 0.0f;
			idata[i * 10 + 7] = 0.0f;
			idata[i * 10 + 8] = 0.0f;
			idata[i * 10 + 9] = 0.0f;
			idata[i * 10 + data[i]] = 1.0f;
		}
	}

	//
	// TRAINING SET LABEL FILE(train - labels - idx1 - ubyte) :
	//
	//	[offset] [type]          [value]          [description]
	//	0000     32 bit integer  0x00000801(2049) magic number(MSB first)
	//	0004     32 bit integer  60000            number of items
	//	0008     unsigned byte   ??               label
	//
	// TRAINING SET IMAGE FILE(train - images - idx3 - ubyte) :
	//
	//	[offset] [type]          [value]          [description]
	//	0000     32 bit integer  0x00000803(2051) magic number
	//	0004     32 bit integer  60000            number of images
	//	0008     32 bit integer  28               number of rows
	//	0012     32 bit integer  28               number of columns
	//	0016     unsigned byte	 ??				  pixel
	//
	//
	//
	// TEST SET LABEL FILE(t10k - labels - idx1 - ubyte) :
	//
	//	[offset] [type]          [value]          [description]
	//	0000     32 bit integer  0x00000801(2049) magic number(MSB first)
	//	0004     32 bit integer  10000            number of items
	//	0008     unsigned byte	 ??				  label
	//	0009     unsigned byte	 ??				  label
	//	........
	//	xxxx     unsigned byte	 ??				  label
	//	The labels values are 0 to 9.
	//
	//	TEST SET IMAGE FILE(t10k - images - idx3 - ubyte) :
	//
	//	[offset] [type]          [value]          [description]
	//	0000     32 bit integer  0x00000803(2051) magic number
	//	0004     32 bit integer  10000            number of images
	//	0008     32 bit integer  28               number of rows
	//	0012     32 bit integer  28               number of columns
	//	0016     unsigned byte	 ??				  pixel
	//	0017     unsigned byte	 ??				  pixel
	//	........
	//	xxxx     unsigned byte	 ??				  pixel
	//	Pixels are organized row - wise.Pixel values are 0 to 255. 0 means background(white), 255 means foreground(black).
	//

	const std::string train_images = "c:\\temp\\train-images";
	const std::string train_labels = "c:\\temp\\train-labels";
	const std::string test_images = "c:\\temp\\t10k-images";
	const std::string test_labels = "c:\\temp\\t10k-labels";

	Matrix<float> trainImages_;
	Matrix<float> trainLabels_;
	Matrix<float> testImages_;
	Matrix<float> testLabels_;
};

#include "unittest.inc"

int main(int argc, char *argv[])
{
	Mnist nist;
	std::vector<unsigned> sizes({ nist.imgSize(), 30, 10 });
	std::unique_ptr<Network<float>> network 
		= std::make_unique<Network<float>>(sizes);

	unsigned epoch = 30;
	const unsigned mini_batch_size = 10;
	float eta = 1.0f;
	float lmbda = 1.0f;

	if (argc > 1) {
		epoch = atoi(argv[1]);
		eta = (float)atof(argv[2]);
		lmbda = (float)atof(argv[3]);
	}

	std::cout << "epoch=" << epoch 
			<< ",mb_size=" << mini_batch_size
			<< ",eta=" << eta
			<< ",lambda=" << lmbda
			<< std::endl;

	network->SGD(nist.trainImages(), 
			nist.trainLabels(), 
			epoch, 
			mini_batch_size, 
			eta, 
			lmbda,
			nist.testImages(), nist.testLabels());

	return 0;
}
